#!/usr/bin/perl -w

package t::IO::Die;

use strict;
use warnings;
use autodie;

use parent qw(
  Test::Class
);

use List::Util qw(reduce);
our ( $a, $b );

use Socket ();
use Errno  ();
use Fcntl;

use Test::More;
use Test::NoWarnings;
use Test::Deep;
use Test::Trap;
use Test::Exception;

use Capture::Tiny ();

use File::Temp ();
use Try::Tiny;

use IO::Die ();

if ( !caller ) {
    my $test_obj = __PACKAGE__->new();
    plan tests => $test_obj->expected_tests(1);
    $test_obj->runtests();
}

#----------------------------------------------------------------------

sub _dummy_user {
    my ($self) = @_;

    if ( !$self->{'_dummy_user'} ) {
        for my $u (qw( nobody daemon )) {
            if ( getpwnam $u ) {
                $self->{'_dummy_user'} = $u;
                last;
            }
        }
    }

    return $self->{'_dummy_user'};
}

sub tempdir {
    return File::Temp::tempdir();
}

sub tempfile {
    my @fh_and_name = File::Temp::tempfile();

    return wantarray ? reverse(@fh_and_name) : $fh_and_name[1];
}

sub _to_bitmask {
    my ( $self, @fhs ) = @_;

    my $mask = q<>;

    for my $fh (@fhs) {
        vec( $mask, fileno($fh), 1 ) = 1;
    }

    return $mask;
}

sub test_open_for_fork_to_perl : Tests(2) {
    my ($self) = @_;

    pipe my $p_rdr, my $c_wtr;

    local $! = 7;

    my $fh;
    my $pid = IO::Die->open( $fh, '|-' );

    if ( !$pid ) {
        IO::Die->close($p_rdr);
        IO::Die->print( $c_wtr, <> );
        exit;
    }

    is( 0 + $!, 7, 'raw fork open() leaves $! alone' );

    IO::Die->close($c_wtr);
    IO::Die->print( $fh, 'haha' );

    {
        local $?;
        IO::Die->close($fh);

        is( <$p_rdr>, 'haha', 'pipe as STDIN works' );
    }

    return;
}

sub test_open_for_fork_from_perl : Tests(2) {
    my ($self) = @_;

    local $! = 7;

    my $fh;
    my $pid = IO::Die->open( $fh, '-|' );

    if ( !$pid ) {
        IO::Die->print('heyhey');
        exit;
    }

    is( 0 + $!, 7, 'raw fork open() leaves $! alone' );

    {
        is( <$fh>, 'heyhey', 'pipe as STDOUT works' );

        local $?;
        IO::Die->close($fh);
    }

    return;
}

sub test_dup_filehandle : Tests(2) {
    my ($self) = @_;

    my ( $file, $wfh ) = $self->tempfile();

    print {$wfh} '123';
    close $wfh;

    IO::Die->open( my $orig_fh, '<', $file );

    IO::Die->open( my $dup_fh, '<&', $orig_fh );

    is( <$dup_fh>, '123', 'open(<&) works' );

    isnt(
        fileno($orig_fh),
        fileno($dup_fh),
        '...and the filehandles are different',
    );

    return;
}

sub test_clone_filehandle : Tests(2) {
    my ($self) = @_;

    my ( $file, $wfh ) = $self->tempfile();

    print {$wfh} '123';
    close $wfh;

    IO::Die->open( my $orig_fh, '<', $file );

    IO::Die->open( my $clone_fh, '<&=', $orig_fh );

    is( <$clone_fh>, '123', 'open(<&) works' );

    is(
        fileno($orig_fh),
        fileno($clone_fh),
        '...and the filehandles are the same file descriptor',
    );

    return;
}

sub test_open_on_a_scalar_ref : Tests(3) {
    my ($self) = @_;

    my $fh;

    my $ok = IO::Die->open( $fh, '<', \123 );
    ok( $ok, 'opened file handle to read from a scalar ref (constant)' );

    is( <$fh>, 123, '...and the file handle reads fine' );

    trap {
        IO::Die->open( $fh, '>', \123 );
    };
    $trap->did_die('error from creating write-to file handle on a scalar ref constant');

    return;
}

sub test_open_on_a_file : Tests(6) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    local $! = 7;

    trap {
        IO::Die->open( my $wfh, '>', "$dir/somefile" );
    };
    $trap->did_return('open(>) on a new file');
    ok( ( -f "$dir/somefile" ), '...and it really did open()' );

    is( 0 + $!, 7, '...and it left $! alone' );

    trap {
        IO::Die->open( my $wfh, ">$dir/somefile" );
    };
    $trap->did_die('open(>) fails on 2-arg');

    trap {
        IO::Die->open( my $wfh, '<', "$dir/otherfile" );
    };
    $trap->did_die('open(<) on a nonexistent file');
    like( $trap->die(), qr<FileOpen>, '...and the error' );

    return;
}

sub test_open_from_a_command : Tests(9) {
    my ($self) = @_;

    my ( $rfh, $pid );

    trap {
        $pid = IO::Die->open( $rfh, '-|', 'echo hi' );
    };
    $trap->did_return('open() from a space-delimited command');
    is( <$rfh>, "hi$/", '...and it really does open() from the command' );
    like( $pid, qr<\A[0-9]+\z>, '...and it returns the PID' );

    trap {
        IO::Die->open( $rfh, '-|', 'echo', 'hi' );
    };
    $trap->did_return('open() from a command with list args');
    is( <$rfh>, "hi$/", '...and it really does open() from the command' );

    trap {
        IO::Die->open( $rfh, '-|', 'echo hi', undef );
    };
    $trap->did_die('open() from a nonexistent command with a space in it');
    like( $trap->die(), qr<Exec>, '..and the exception' );

    my $dir = $self->tempdir();

    trap {
        IO::Die->open( $rfh, '-|', "$dir/hahaha" );
    };
    $trap->did_die('open() from a nonexistent command');
    like( $trap->die(), qr<Exec>, '..and the exception' );

    return;
}

sub test_open_to_a_command : Tests(9) {
    my ($self) = @_;

    my ( $rfh, $pid );

    my $stdout;

    #Test::Trap doesn't play nicely with Capture::Tiny.
    local $@;

    eval {
        $stdout = Capture::Tiny::capture_stdout(
            sub {
                $pid = IO::Die->open( $rfh, '|-', 'perl -e "print <>"' );
                IO::Die->print( $rfh, 'ohyeah' );
                IO::Die->close($rfh);
            }
        );
    };
    ok( !$@, 'open() from a space-delimited command' );
    is( $stdout, "ohyeah", '...and it really does open() to the command' );
    like( $pid, qr<\A[0-9]+\z>, '...and it returns the PID' );

    eval {
        $stdout = Capture::Tiny::capture_stdout(
            sub {
                IO::Die->open( $rfh, '|-', 'perl', -e => 'print 123; print <>' );
                IO::Die->print( $rfh, 'ohyeah' );
                IO::Die->close($rfh);
            }
        );
    };
    ok( !$@, 'open() to a command with list args' );
    is( $stdout, '123ohyeah', '...and it obeys parameters and still open()s to the command' );

    trap {
        IO::Die->open( $rfh, '|-', 'echo hi', undef );
    };
    $trap->did_die('open() to a nonexistent command with a space in it');
    like( $trap->die(), qr<Exec>, '..and the exception' );

    my $dir = $self->tempdir();

    trap {
        IO::Die->open( $rfh, '|-', "$dir/hahaha" );
    };
    $trap->did_die('open() to a nonexistent command');
    like( $trap->die(), qr<Exec>, '..and the exception' );

    return;
}

sub test_sysopen : Tests(7) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    my ( $opened, $fh );
    trap {
        $opened = IO::Die->sysopen( $fh, "$dir/notthere", Fcntl::O_RDONLY );
    };
    $trap->did_die('sysopen(O_RDONLY) on a nonexistent file');
    like( $trap->die(), qr<FileOpen>, '..and the exception' ) or diag explain $trap->die();

    local $! = 7;

    $fh = IO::Handle->new();

    local $@;
    trap {
        $opened = IO::Die->sysopen( $fh, "$dir/i_am_here", Fcntl::O_CREAT | Fcntl::O_WRONLY, 0600 );
    };
    $trap->did_return('sysopen(O_CREAT | O_WRONLY) on a nonexistent file');
    ok( $opened, '..and the return is truthy' );
    isa_ok( $fh, 'GLOB', '...and auto-vivification works' );

    is( 0 + $!, 7, '...and it left $! alone' );

    syswrite( $fh, '7' );
    is( ( -s $fh ), 1, '...and the filehandle is a write filehandle' );

    return;
}

sub test_read : Tests(9) {
    my ($self) = @_;

    return $self->_test_read_func( \&IO::Die::read );
}

sub test_sysread : Tests(10) {
    my ($self) = @_;

    $self->_test_read_func( \&IO::Die::sysread );

    #Let's check sysread()'s unbuffered-ness.

    my $dir = $self->tempdir();
    open my $fh, '+>', "$dir/somefile";

    trap {
        my $buffer = q<>;
        for ( 1 .. 100 ) {
            my $random = rand;

            sysseek( $fh, 0, 0 );
            truncate $fh, 0;
            syswrite( $fh, $random );
            sysseek( $fh, 0, 0 );
            IO::Die->sysread( $fh, $buffer, length $random );

            die if $buffer ne $random;
        }
    };
    $trap->did_return('sysread() is really unbuffered') or diag explain $trap;

    return;
}

sub _test_read_func {
    my ( $self, $func_cr ) = @_;

    my ( $file, $fh ) = $self->tempfile();

    my $alphabet = q<>;
    $alphabet .= $_ for 'a' .. 'z';
    print {$fh} $alphabet;

    close $fh;

    open $fh, '<', $file;

    my $buffer = q<>;

    local $! = 7;

    trap {
        $func_cr->( 'IO::Die', $fh, $buffer, 2 );
    };
    $trap->did_return('read succeeded');
    is( $buffer, 'ab', '...and actually worked' );

    is( 0 + $!, 7, '...and it left $! alone' );

    $buffer .= '12345';
    my $bytes = $func_cr->( 'IO::Die', $fh, $buffer, 2, -3 );
    is( $buffer, 'ab12cd', 'read obeys OFFSET' );
    is( $bytes,  2,        '...and it returns the number of bytes read' );

    $bytes = $func_cr->( 'IO::Die', $fh, $buffer, 100_000 );
    is( $buffer, 'efghijklmnopqrstuvwxyz', 'read when LENGTH is over size' );
    is( $bytes,  length($buffer),          '...and the number of bytes is correct' );

    close $fh;

    trap {
        $func_cr->( 'IO::Die', $fh, $buffer, 7 );
    };
    like( $trap->die(), qr<Read>, 'error read on a closed filehandle' );
    like( $trap->die(), qr<7>,    '...and the error has the intended number of bytes' );

    return;
}

sub test_print_with_filehandle : Tests(10) {
    my ($self) = @_;

    my ( $file, $fh ) = $self->tempfile();

    local $! = 7;

    my $printed;
    trap {
        $printed = IO::Die->print( $fh, 'ha', 'ha' );
    };
    $trap->did_return('print() to a file with a given string');
    ok( $printed, '...and it returns a true value' );
    is( do { local $!; scalar `cat $file` }, 'haha', '...and the print actually happened' );

    is( 0 + $!, 7, '...and it left $! alone' );

    for ('hoho') {
        trap {
            $printed = IO::Die->print($fh);
        };
        $trap->did_return('print() to a file from $_');
        ok( $printed, '...and it returns a true value' );
        is( ( scalar `cat $file` ), 'hahahoho', '...and the print actually happened' );
    }

    close $fh;

    open my $rfh, '<', $file;
    trap {
        IO::Die->print($rfh) for 'haha!';
    };
    $trap->did_die('print() dies when writing to a non-write filehandle');
    like( $trap->die(), qr<Write>, '...and the exception' );
    like( $trap->die(), qr<5>,     '...and the exception contains the total number of bytes' );

    return;
}

sub test_print_without_filehandle : Tests(9) {
    my ($self) = @_;

    {
        my ( $file, $fh ) = $self->tempfile();

        my $orig_fh = $self->_overwrite_stdout($fh);

        try {
            my $printed;
            trap {
                $printed = IO::Die->print('haha');
            };
            $trap->did_return('print() to a file with a given string');
            ok( $printed, '...and it returns a true value' );
            is( ( scalar `cat $file` ), 'haha', '...and the print actually happened' );

            for ('hoho') {
                trap {
                    $printed = IO::Die->print();
                };
                $trap->did_return('print() to a file from $_');
                ok( $printed, '...and it returns a true value' );
                is( ( scalar `cat $file` ), 'hahahoho', '...and the print actually happened' );
            }

            close $fh;

            trap {
                $printed = IO::Die->print( 'I', 'die' );
            };
        }
        finally {
            select $orig_fh;
        };
    }

    $trap->did_die('print() dies when the filehandle is closed');
    like( $trap->die(), qr<Write>, '...and the exception' );
    like( $trap->die(), qr<4>,     '...and the exception contains the total number of bytes' );

    return;
}

sub test_syswrite : Tests(14) {
    my ($self) = @_;

    my ( $file, $fh ) = $self->tempfile();

    local $! = 7;

    my $printed;
    trap {
        $printed = IO::Die->syswrite( $fh, 'haha' );
    };
    $trap->did_return('write to a file with a given string');

    is( 0 + $!, 7, '...and it left $! alone' );

    is( $printed, 4, '...and it returns the number of bytes' );
    is( ( scalar `cat $file` ), 'haha', '...and the write actually happened' );

    IO::Die->syswrite( $fh, 'haha', 1 );
    is( ( scalar `cat $file` ), 'hahah', 'We obey LENGTH' );

    IO::Die->syswrite( $fh, 'haha', 1, 1 );
    is( ( scalar `cat $file` ), 'hahaha', 'We obey OFFSET' );

    IO::Die->syswrite( $fh, 'abcdefg', 1, -3 );
    is( ( scalar `cat $file` ), 'hahahae', 'We obey negative OFFSET' );

    close $fh;

    open my $rfh, '<', $file;
    trap {
        IO::Die->syswrite( $rfh, 'abcde' );
    };
    like( $trap->die(), qr<Write>, 'exception when writing to a non-write filehandle' );
    like( $trap->die(), qr<5>,     '...and the exception contains the number of bytes meant to be written' );

    trap {
        IO::Die->syswrite( $rfh, 'abcde', 2 );
    };
    like( $trap->die(), qr<2>, 'The exception contains the correct number of bytes meant to be written if there was a LENGTH' );

    trap {
        IO::Die->syswrite( $rfh, 'abcde', 200 );
    };
    like( $trap->die(), qr<5>, 'The exception contains the correct number of bytes meant to be written if there was an over-long LENGTH' );

    trap {
        IO::Die->syswrite( $rfh, 'abcde', 2, 1 );
    };
    like( $trap->die(), qr<2>, 'The exception contains the correct number of bytes meant to be written if there was a LENGTH and positive OFFSET' );

    trap {
        IO::Die->syswrite( $rfh, 'abcde', 200, 1 );
    };
    like( $trap->die(), qr<4>, 'The exception contains the correct number of bytes meant to be written if there was an over-long LENGTH and positive OFFSET' );

    trap {
        IO::Die->syswrite( $rfh, 'abcde', 200, -3 );
    };
    like( $trap->die(), qr<3>, 'The exception contains the correct number of bytes meant to be written if there was an over-long LENGTH and negative OFFSET' );

    return;
}

sub test_close_with_filehandle : Tests(6) {
    my ($self) = @_;

    my ( $file, $fh ) = $self->tempfile();

    local $! = 7;

    my $closed;
    trap {
        $closed = IO::Die->close($fh);
    };
    $trap->did_return('close()');
    ok( $closed,            '...and the return value is truthy' );
    ok( !CORE::fileno($fh), '...and the filehandle actually closed' );

    is( 0 + $!, 7, '...and it left $! alone' );

    trap {
        IO::Die->close($fh);
    };
    $trap->did_die('close() dies when the filehandle is already closed');
    like( $trap->die(), qr<Close>, '...and the exception' );

    return;
}

sub test_close_without_filehandle : Tests(5) {
    my ($self) = @_;

    my ( $fh, $closed );

    {
        ( undef, $fh ) = $self->tempfile();

        my $orig_fh = $self->_overwrite_stdout($fh);

        try {
            select $fh;    ## no critic qw(ProhibitOneArgSelect)
            close $fh;

            trap {
                $closed = IO::Die->close();
            };
        }
        finally {
            select $orig_fh;
        };
    }
    $trap->did_die('close() dies if the select()ed filehandle is already closed');
    like( $trap->die(), qr<Close>, '...and the exception' );

    {
        ( undef, $fh ) = $self->tempfile();

        my $orig_fh = $self->_overwrite_stdout($fh);

        try {
            select $fh;    ## no critic qw(ProhibitOneArgSelect)

            trap {
                $closed = IO::Die->close();
            };
        }
        finally {
            select $orig_fh;
        };
    }

    $trap->did_return('close()');
    ok( $closed,            '...and the return value is truthy' );
    ok( !CORE::fileno($fh), '...and the filehandle actually closed' );

    return;
}

sub test_seek : Tests(5) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    open my $fh, '+>', "$dir/file";
    print {$fh} 'a' .. 'z';

    my $buffer;
    my $sought;

    local $! = 7;

    $sought = IO::Die->seek( $fh, 0, 0 );
    ok( $sought, 'returns a truthy value' );

    is( 0 + $!, 7, '...and it left $! alone' );

    read( $fh, $buffer, 1 );
    is( $buffer, 'a', '...and it went to the beginning' );

    $sought = IO::Die->seek( $fh, -1, Fcntl::SEEK_END );
    read( $fh, $buffer, 1 );
    is( $buffer, 'z', 'seek() to one from the end' );
    ok( $sought, '...and it returns a truthy value' );

    return;
}

sub test_sysseek : Tests(5) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    open my $fh, '+>', "$dir/file";
    syswrite( $fh, $_ ) for 'a' .. 'z';

    my $buffer;
    my $sought;

    local $! = 7;

    $sought = IO::Die->sysseek( $fh, 0, 0 );
    ok( $sought, 'returns a truthy value' );

    is( 0 + $!, 7, '...and it left $! alone' );

    sysread( $fh, $buffer, 1 );
    is( $buffer, 'a', '...and it went to the beginning' );

    $sought = IO::Die->sysseek( $fh, -1, Fcntl::SEEK_END );
    sysread( $fh, $buffer, 1 );
    is( $buffer, 'z', 'seek to one from the end' );
    ok( $sought, '...and it returns a truthy value' );

    return;
}

sub test_truncate : Tests(10) {
    my ($self) = @_;

    my @letters = 'a' .. 'z';
    my $alphabet = reduce { $a . $b } @letters;

    my ( $file, $fh ) = $self->tempfile();
    print {$fh} @letters;

    seek( $fh, 0, Fcntl::SEEK_CUR );

    local $! = 7;

    my $trunc = IO::Die->truncate( $fh, 10 );
    ok( $trunc, 'truncate() on a filehandle returns truthy' );

    is( 0 + $!, 7, '...and it left $! alone' );

    is( do { local $!; scalar `cat $file` }, substr( $alphabet, 0, 10 ), 'truncate() does its thing' );

    IO::Die->close($fh);

    IO::Die->open( $fh, '<', $file );

    trap {
        IO::Die->truncate( $fh, 10 );
    };
    like( $trap->die(), qr<FileTruncate>, 'error from truncating on read-only filehandle' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::EINVAL() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    $trunc = IO::Die->truncate( $file, 1000 );
    ok( $trunc, 'truncate() returns truthy when truncating a filename' );

    is( ( -s $file ), 1000, '...and the “truncate” to a larger-than-previous size works' );

    IO::Die->unlink($file);

    trap {
        IO::Die->truncate( $file, 10 );
    };
    like( $trap->die(), qr<FileTruncate>, 'error from truncating nonexistent file' );

    $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_opendir : Tests(6) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    local $! = 7;

    my $res = IO::Die->opendir( my $dfh, $dir );
    ok( $res, 'return value' );
    isa_ok( $dfh, 'GLOB', 'auto-vivify' );

    is( 0 + $!, 7, '...and it left $! alone' );

    trap {
        IO::Die->opendir( my $dfh, "$dir/not_there" );
    };
    like( $trap->die(), qr<DirectoryOpen>, 'error from opening nonexistent directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_rewinddir : Tests(5) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    my %struct = (
        alpha   => 1,
        beta    => 2,
        gamma   => 3,
        delta   => 4,
        epsilon => 5,
    );
    while ( my ( $fn, $cont ) = each %struct ) {
        open my $fh, '>', "$dir/$fn";
        print {$fh} $cont or die $!;
        close $fh;
    }

    IO::Die->opendir( my $dfh, $dir );

    local $!;

    do { readdir $dfh }
      for ( 1 .. 4 );

    $! = 7;

    IO::Die->rewinddir($dfh);

    is( 0 + $!, 7, 'rewinddir() leaves $! alone' );

    cmp_bag(
        [ grep { !tr<.><> } readdir $dfh ],
        [qw( alpha beta gamma delta epsilon )],
        'rewinddir() did actually rewind the directory',
    );

    IO::Die->closedir($dfh);

    $! = 7;

    trap {
        IO::Die->rewinddir($dfh);
    };
    like( $trap->die(), qr<DirectoryRewind>, 'error from closing already-closed directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::EBADF() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_closedir : Tests(5) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    IO::Die->opendir( my $dfh, $dir );

    local $! = 7;

    my $res = IO::Die->closedir($dfh);
    ok( $res, 'return value' );

    is( 0 + $!, 7, '...and it left $! alone' );

    trap {
        IO::Die->closedir($dfh);
    };
    like( $trap->die(), qr<DirectoryClose>, 'error from closing already-closed directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::EBADF() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_unlink : Tests(10) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `touch $dir/redshirt$_` for ( 0 .. 9 );

    local $! = 7;

    my $ok = IO::Die->unlink("$dir/redshirt0");
    is( $ok, 1, 'returns 1 if one path unlink()ed' );
    ok( !do { local $!; -e "$dir/redshirt0" }, '...and the unlink() worked' );

    is( 0 + $!, 7, '...and it left $! alone' );

    $ok = IO::Die->unlink() for ("$dir/redshirt9");
    is( $ok, 1, 'returns 1 if one path unlink()ed (via $_)' );
    ok( !do { local $!; -e "$dir/redshirt0" }, '...and the unlink() worked (via $_)' );

    trap {
        IO::Die->unlink( "$dir/redshirt1", "$dir/redshirt2" );
    };
    $trap->did_die('die()d with >1 path passed');
    ok( do { local $!; -e "$dir/redshirt1" }, '...and the unlink() did NOT happen' );

    trap {
        IO::Die->unlink("$dir/redshirt0");
    };
    like( $trap->die(), qr<Unlink>, 'failure when unlink()ing a nonexistent file' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub _errno_to_str {
    my ( $self, $num ) = @_;

    local $! = $num;

    return "$!";
}

sub test_mkdir : Tests(10) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    local $! = 7;

    my $ok;

    $ok = IO::Die->mkdir() for ("$dir/dollar_under");
    is( $ok, 1, 'returns 1 if mkdir() with no args' );
    ok( do { local $!; -e "$dir/dollar_under" }, '...and the mkdir() worked' );

    is( 0 + $!, 7, '...and it left $! alone' );

    $ok = IO::Die->mkdir("$dir/one_arg");
    is( $ok, 1, 'returns 1 if one path mkdir()ed' );
    ok( do { local $!; -e "$dir/one_arg" }, '...and the mkdir() worked' );

    $ok = IO::Die->mkdir( "$dir/with_perms", 0111 );
    is( $ok, 1, 'returns 1 if one path mkdir()ed with perms' );
    ok( do { local $!; -e "$dir/with_perms" }, '...and the mkdir() worked' );

    trap {
        IO::Die->mkdir("$dir/not_there/not_a_chance");
    };
    like( $trap->die(), qr<DirectoryCreate>, 'failure when mkdir()ing a directory in a nonexistent directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_rmdir : Tests(10) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `mkdir $dir/redshirt$_` for ( 0 .. 9 );

    local $! = 7;

    my $ok = IO::Die->rmdir("$dir/redshirt0");
    is( $ok, 1, 'returns 1 if one path rmdir()ed' );
    ok( !do { local $!; -e "$dir/redshirt0" }, '...and the rmdir() worked' );

    is( 0 + $!, 7, '...and it left $! alone' );

    $ok = IO::Die->rmdir() for ("$dir/redshirt9");
    is( $ok, 1, 'returns 1 if one path rmdir()ed (via $_)' );
    ok( !do { local $!; -e "$dir/redshirt0" }, '...and the rmdir() worked (via $_)' );

    trap {
        IO::Die->rmdir( "$dir/redshirt1", "$dir/redshirt2" );
    };
    $trap->did_die('die()d with >1 path passed');
    ok( do { local $!; -e "$dir/redshirt1" }, '...and the rmdir() did NOT happen' );

    trap {
        IO::Die->rmdir("$dir/redshirt0");
    };
    like( $trap->die(), qr<DirectoryDelete>, 'failure when rmdir()ing a nonexistent directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_chmod : Tests(12) {
    my ($self) = @_;

    my $dir  = $self->tempdir();
    my $dir2 = $self->tempdir();

    my ( $file, $fh ) = $self->tempfile();

    local $! = 7;

    my $ok = IO::Die->chmod( 0111, $dir );
    is( $ok, 1, 'returns 1 if one path chmod()ed' );
    is( 0777 & ( IO::Die->stat($dir) )[2], 0111, '...and the chmod() worked' );

    is( 0 + $!, 7, '...and it left $! alone' );

    trap {
        IO::Die->chmod( 0222, $dir, $dir2 );
    };
    $trap->did_die('die()d with >1 path passed');
    is( 0777 & ( IO::Die->stat($dir) )[2], 0111, '...and the chmod() did NOT happen' );

    $ok = IO::Die->chmod( 0321, $fh );
    is( $ok, 1, 'returns 1 if one filehandle chmod()ed' );
    is( 0777 & ( IO::Die->stat($fh) )[2], 0321, '...and the chmod() worked' );

    IO::Die->close($fh);

    trap {
        IO::Die->chmod( 0456, $fh );
    };
    like( $trap->die(), qr<Chmod>, 'failure when chmod()ing a closed filehandle' );

    is( 0 + $!, 7, '...and it left $! alone' );

  TODO: {
        local $TODO = 'https://rt.perl.org/Ticket/Display.html?id=122703';

        my $str = $self->_errno_to_str( Errno::ENOTTY() );

        like(
            $trap->die(),
            qr<$str>,
            "exception’s error()",
        ) or diag explain $trap->die();
    }

    trap {
        IO::Die->chmod( 0456, "$dir/not_there" );
    };
    like( $trap->die(), qr<Chmod>, 'failure when chmod()ing a nonexistent file' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_chown : Tests(12) {
    my ($self) = @_;

    my $nobody_uid = ( getpwnam $self->_dummy_user() )[2];
    my $nobody_gid = ( getgrnam $self->_dummy_user() )[2];

  SKIP: {
        skip 'Need *nix OS for tests', $self->num_tests() if !$nobody_uid;
        skip 'Must be root!',          $self->num_tests() if $>;

        my $dir  = $self->tempdir();
        my $dir2 = $self->tempdir();

        local $!;

        $! = 7;

        my $ok = IO::Die->chown( $nobody_uid, -1, $dir );
        is( $ok, 1, 'returns 1 if one path chown()ed' );
        is( ( IO::Die->stat($dir) )[4], $nobody_uid, '...and the chown() worked' );

        is( 0 + $!, 7, '...and it left $! alone' );

        trap {
            IO::Die->chown( -1, $nobody_gid, $dir, $dir2 );
        };
        $trap->did_die('die()d with >1 path passed');
        is( ( IO::Die->stat($dir) )[5], 0 + $), '...and the chown() did NOT happen' );

        my ( $file, $fh ) = $self->tempfile();

        $ok = IO::Die->chown( -1, $nobody_gid, $fh );
        is( $ok, 1, 'returns 1 if one filehandle chown()ed' );
        is( ( IO::Die->stat($fh) )[5], $nobody_gid, '...and the chown() worked' ) or diag explain [ IO::Die->stat($fh) ];

        IO::Die->close($fh);

        trap {
            IO::Die->chown( $>, 0 + $), $fh );
        };
        like( $trap->die(), qr<Chown>, 'failure when chown()ing a closed filehandle' );

        is( 0 + $!, 7, '...and it left $! alone' );

      TODO: {
            local $TODO = 'https://rt.perl.org/Ticket/Display.html?id=122703';

            my $str = $self->_errno_to_str( Errno::ENOTTY() );

            like(
                $trap->die(),
                qr<$str>,
                "exception’s error()",
            ) or diag explain $trap->die();
        }

        trap {
            IO::Die->chown( $>, 0 + $), "$dir/not_there" );
        };
        like( $trap->die(), qr<Chown>, 'failure when chown()ing a nonexistent file' );

        my $str = $self->_errno_to_str( Errno::ENOENT() );

        like(
            $trap->die(),
            qr<$str>,
            "exception’s error()",
        ) or diag explain $trap->die();
    }

    return;
}

sub test_stat : Tests(6) {
    my ($self) = @_;

    my $file = $self->tempfile();

    my @stat = stat $file;

    local $! = 7;

    my $scalar = IO::Die->stat($file);
    ok( $scalar, 'stat() in scalar context (with a filename) returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    IO::Die->unlink($file);

    is_deeply(
        [ IO::Die->stat( \*_ ) ],
        \@stat,
        'stat() in list context (and using “\*_”) returns the (cached) stat data',
    );

    trap {
        IO::Die->stat($file);
    };
    like( $trap->die(), qr<Stat>, 'failure when stat()ing a nonexistent path' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_lstat : Tests(7) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `touch $dir/empty`;

    symlink 'empty', "$dir/symlink";

    my @file_stat = lstat "$dir/empty";
    my @link_stat = lstat "$dir/symlink";

    #sanity
    die "huh?" if "@file_stat" ne join( ' ', stat "$dir/symlink" );

    local $! = 7;

    my $scalar = IO::Die->lstat("$dir/empty");
    ok( $scalar, 'lstat() in scalar context (with a filename) returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    is_deeply(
        [ IO::Die->lstat("$dir/symlink") ],
        \@link_stat,
        'lstat() (in list context) finds the symlink and stats that, not the referant file',
    );

    IO::Die->unlink("$dir/symlink");

    #warnings.pm will complain about lstat(_).
    {
        $SIG{'__WARN__'} = sub { };

        is_deeply(
            [ IO::Die->lstat( \*_ ) ],
            \@link_stat,
            'lstat() reads the cache when passed in “\*_”',
        );
    }

    trap {
        IO::Die->lstat("$dir/symlink");
    };
    like( $trap->die(), qr<Stat>, 'failure when lstat()ing a nonexistent symlink' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_link : Tests(6) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `touch $dir/file`;

    local $! = 7;

    my $scalar = IO::Die->link( "$dir/file", "$dir/hardlink" );
    ok( $scalar, 'link() returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    {
        local $!;

        my @orig_file = lstat "$dir/file";
        my @the_link  = lstat "$dir/hardlink";

        is_deeply( \@the_link, \@orig_file, 'new hardlink is the same filesystem node as the old filename' );
    }

    trap {
        IO::Die->link( "$dir/notthere", "$dir/not_a_chance" );
    };
    like( $trap->die(), qr<Link>, 'failure when link()ing to a nonexistent file' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_symlink : Tests(7) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `touch $dir/file`;

    local $! = 7;

    my $scalar = IO::Die->symlink( "file", "$dir/symlink" );
    ok( $scalar, 'symlink() returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    {
        local $!;

        my @orig_file = stat "$dir/file";
        my @the_link  = stat "$dir/symlink";

        is_deeply( \@the_link, \@orig_file, 'new symlink points to the same filesystem node as the old filename' );
    }

    $scalar = IO::Die->symlink( "notthere", "$dir/not_a_chance" );
    ok( $scalar, 'symlink() even lets you create a dangling symlink' );

    trap {
        IO::Die->symlink( "notthere", "$dir/not_a_dir/not_a_chance" );
    };
    like( $trap->die(), qr<SymlinkCreate>, 'failure when creating a symlink() in a nonexistent directory' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_readlink : Tests(10) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    local $!;

    symlink 'haha', "$dir/mylink";

    `touch $dir/myfile`;

    $! = 7;

    my $scalar = IO::Die->readlink("$dir/mylink");
    is( $scalar, 'haha', 'readlink() returns the link’s value (i.e., destination)' );
    is( 0 + $!,  7,      '...and it left $! alone' );

    for ("$dir/mylink") {
        my $scalar = IO::Die->readlink();
        is( $scalar, 'haha', 'readlink() respects $_ if no parameter is passed' );

        no warnings 'uninitialized';
        trap { IO::Die->readlink(undef) };
        $trap->did_die('...but if undef is passed, then we do NOT read $_');
        my $str = $self->_errno_to_str( Errno::ENOENT() );

        like(
            $trap->die(),
            qr<$str>,
            "exception’s error()",
        ) or diag explain $trap->die();

        is( 0 + $!, 7, '...and it left $! alone' );
    }

    trap {
        IO::Die->readlink("$dir/myfile");
    };
    like( $trap->die(), qr<SymlinkRead>, 'failure when reading a symlink that’s actually a file' );

    my $str = $self->_errno_to_str( Errno::EINVAL() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    trap {
        IO::Die->readlink("$dir/not_there");
    };
    like( $trap->die(), qr<SymlinkRead>, 'failure when reading a nonexistent symlink' );

    $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_rename : Tests(6) {
    my ($self) = @_;

    my $dir = $self->tempdir();

    `touch $dir/file`;

    local $! = 7;

    my $scalar = IO::Die->rename( "$dir/file", "$dir/file2" );
    ok( $scalar, 'rename() returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    {
        local $!;
        ok( ( -e "$dir/file2" ), "rename() actually renamed the file" );
    }

    trap {
        IO::Die->rename( "$dir/not_there", "$dir/not_at_all" );
    };
    like( $trap->die(), qr<Rename>, 'failure when rename()ing a nonexistent file' );

    is( 0 + $!, 7, '...and it left $! alone' );

    my $str = $self->_errno_to_str( Errno::ENOENT() );

    like(
        $trap->die(),
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return;
}

sub test_exec : Tests(3) {
    my ($self) = @_;

    my $scratch = $self->tempdir();

    trap {
        IO::Die->exec("$scratch/not_there");
    };
    like( $trap->die(), qr<Exec>, 'error type' );
    my $str = $self->_errno_to_str( Errno::ENOENT() );
    like( $trap->die(), qr<$str>, 'error text' );

  SKIP: {
        skip 'This test requires a *nix OS!', 1 if !`which echo`;

        open my $script_fh, '>', "$scratch/ha ha ha";
        print {$script_fh} "#!/bin/sh$/echo oyoyoy$/";
        chmod 0755, $script_fh;
        close $script_fh;

        pipe my $rfh, my $wfh;
        my $pid = fork || do {
            close $rfh;
            open \*STDOUT, '>&=' . fileno($wfh);
            IO::Die->exec("$scratch/ha ha ha");
            exit 1;    #just in case
        };
        close $wfh;
        my $child_out = do { local $/; <$rfh> };
        close $rfh;

        is( $child_out, "oyoyoy$/", 'exec() handles a command with a space and no args safely' );
    }

    return;
}

sub test_fork : Tests(1) {
    my $pid = IO::Die->fork() || do { exit };
    ok( $pid, 'PID returned' );
    waitpid $pid, 0;

    return;
}

sub test_fork_failure : Tests(2) {
    my ($self) = @_;

    my $uid = ( getpwnam $self->_dummy_user() )[2];
    my $gid = ( getgrnam $self->_dummy_user() )[2];

    local $@;

  SKIP: {
        skip 'Need *nix OS for tests!', $self->num_tests() if !$uid;
        skip 'Need BSD::Resource',      $self->num_tests() if !eval { require BSD::Resource; };
        skip 'Must be root!',           $self->num_tests() if $>;

        pipe my $rdr, my $wtr;

        my $pid = fork || do {
            try {
                $> = $uid;
                $) = $gid;
                $< = $uid;

                BSD::Resource::setrlimit( BSD::Resource::RLIMIT_NPROC(), 3, 3 ) or die "setrlimit()";

                close $rdr;

                my $main_pid = $$;

                local $SIG{'__DIE__'} = sub {
                    exit 1 if $$ != $main_pid;
                    my $err = shift;
                    print {$wtr} "$err$/";
                    exit 1;
                };

                IO::Die->fork() while 1;
            }
            finally {
                exit;
            }
        };

        close $wtr;
        my $child_out = do { local $/; <$rdr> };
        close $rdr;

        like(
            $child_out,
            qr<Fork>,
            "exception’s type",
        ) or diag explain [$child_out];

        my $str = $self->_errno_to_str( Errno::EAGAIN() );

        like(
            $child_out,
            qr<$str>,
            "exception’s error()",
        ) or diag explain [$child_out];
    }

    return;
}

sub test_pipe : Tests(5) {
    local $! = 7;

    my $ok = IO::Die->pipe( my $rdr, my $wtr );
    ok( $ok, 'pipe() returns something truthy' );
    is( 0 + $!, 7, '...and it left $! alone' );

    isa_ok( $rdr, 'GLOB', 'auto-vivify the reader' );
    isa_ok( $wtr, 'GLOB', 'auto-vivify the writer' );

    my $pid = fork || do {
        close $rdr;
        print $wtr 42;
        exit;
    };

    close $wtr;

    my $from_child = <$rdr>;
    close $rdr;

    do { local $?; waitpid $pid, 0 };

    is( $from_child, 42, '...and the pipe really works' );

    return;
}

sub test_pipe_failure : Tests(2) {
    my ($self) = @_;

    my $uid = ( getpwnam $self->_dummy_user() )[2];
    my $gid = ( getgrnam $self->_dummy_user() )[2];

  SKIP: {
        skip 'Need *nix OS for tests!', $self->num_tests() if !$uid;
        skip 'Must be root!',           $self->num_tests() if $>;

        pipe my $rdr, my $wtr;

        my $pid = fork || do {
            try {
                $> = $uid;
                $) = $gid;
                $< = $uid;

                close $rdr;
                alarm 60;
                my @pipes;

                local $SIG{'__DIE__'} = sub {
                    my $err = shift;

                    close $_ for splice @pipes;

                    print {$wtr} "$err$/";

                    exit 1;
                };

                while (1) {
                    IO::Die->pipe( my $rdr, my $wtr );
                    push @pipes, $rdr, $wtr;
                }
            }
            finally {
                exit;
            };
        };

        close $wtr;
        my $child_out = do { local $/; <$rdr> };
        close $rdr;

        do { local $?; waitpid $pid, 0 };

        my $str = $self->_errno_to_str( Errno::EMFILE() );

        like(
            $child_out,
            qr<Pipe>,
            "exception’s type",
        ) or diag explain [$child_out];

        like(
            $child_out,
            qr<$str>,
            "exception’s error()",
        ) or diag explain [$child_out];
    }

    return;
}

sub test_fcntl : Tests(5) {
    my ($self) = @_;

    my ( $file, $fh ) = $self->tempfile();

    local $! = 7;

    is(
        IO::Die->fcntl( $fh, &Fcntl::F_GETFL, 0 ),
        do { local $!; fcntl( $fh, &Fcntl::F_GETFL, 0 ) },
        'flags on a “normal” write-only Perl filehandle',
    );

    is( 0 + $!, 7, '...and it left $! alone' );

    {
        local $!;
        sysopen( my $fh2, $file, &Fcntl::O_RDONLY );
        is(
            IO::Die->fcntl( $fh2, &Fcntl::F_SETFL, &Fcntl::O_NONBLOCK ),
            '0 but true',
            'response from fcntl(F_SETFL)',
        );
    }

    close $fh;

    trap {
        IO::Die->fcntl( $fh, &Fcntl::F_GETFL, 0 );
    };
    like( $trap->die(), qr<Fcntl>, 'error from fcntl() on closed filehandle' );

    is( 0 + $!, 7, '...and it left $! alone' );

    return;
}

sub test_systell : Tests(3) {
    my ($self) = @_;

    my ( $tempfile, $tfh ) = $self->tempfile();

    is(
        IO::Die->systell($tfh),
        0,
        'systell() is 0 for start of file',
    );

    IO::Die->syswrite( $tfh, 'haha' );

    is(
        IO::Die->systell($tfh),
        4,
        'systell() after writing',
    );

    close $tfh;

    trap {
        IO::Die->systell($tfh);
    };
    $trap->did_die('systell() on a closed filehandle die()s');

    return;
}

sub test_select : Tests(12) {
    my ($self) = @_;

    my ( $rdr, $wtr );
    IO::Die->pipe( $rdr, $wtr );

    local $! = 5;

    my $rdr_mask = $self->_to_bitmask($rdr);
    my $wtr_mask = $self->_to_bitmask($wtr);

    my ( $number, $timeleft ) = IO::Die->select(
        my $rbits = $rdr_mask,
        my $wbits = $wtr_mask,
        undef, 60,
    );

    is( $number, 1, 'correct # returned' );
    like( $timeleft, qr<\A[0-9]+(?:\.[0-9]+)?\z>, 'correct timeleft returned' );

    is( 0 + $!,  5, '$! is left alone' );
    is( 0 + $^E, 5, '$^E is left alone' );

    like( $rbits, qr<\A\0+\z>, 'initial read bits on a single-process pipe' );
    is( $wbits, $wtr_mask, 'initial write bits on a single-process pipe' );

    IO::Die->syswrite( $wtr, 'haha' );

    my $scalar = IO::Die->select(
        $rbits = $rdr_mask,
        $wbits = $wtr_mask,
        undef, undef,
    );

    is( $rbits,  $rdr_mask, 'after the pipe buffer has data, now read bits are different' );
    is( $scalar, 2,         '...as is the number of ready handles (returned as a scalar)' );

    is( $wbits, $wtr_mask, '...but write bits are the same' );

    close $rdr;

    dies_ok(
        sub {
            IO::Die->select(
                $rbits = $rdr_mask,
                undef, undef, undef,
            );
        },
        'exception from select()ing on a closed filehandle',
    );
    my $err = $@;
    like( $err, qr<Select>, '...and the exception type' );

    my $str = $self->_errno_to_str( Errno::EBADF() );

    like(
        $err,
        qr<$str>,
        "exception’s error()",
    ) or diag explain $trap->die();

    return 1;
}

sub test_select_multiplex : Tests(1) {
    my ($self) = @_;

    #----------------------------------------------------------------------
    #NOTE: The below is just to verify that multiplexing works.
    #There are no meaningful assertions because the success/failure
    #is whether the while {} block below ever finishes.

    my ( $pread, $pwrite, $cread, $cwrite );
    IO::Die->pipe( $pread, $cwrite );
    IO::Die->pipe( $cread, $pwrite );

    my $pid = fork || do {
        close $_ for ( $pread, $pwrite );
        print {$cwrite} readline $cread;
        close $_ for ( $cread, $cwrite );
        exit;
    };

    IO::Die->close($_) for ( $cread, $cwrite );

    my $message = "The quick brown fox jumps over the lazy dog." x 1000;
    $message .= "\n";

    my $prmask    = $self->_to_bitmask($pread);
    my $pwmask    = $self->_to_bitmask($pwrite);
    my $sent_back = q<>;
    my $written;
    while ( $sent_back ne $message ) {
        my @ret = IO::Die->select(
            my $rbits = $prmask,
            my $wbits = $written ? undef : $pwmask,
            undef,
            undef,
        );

        if ( !$written && $wbits && ( $wbits & $pwmask ) eq $pwmask ) {
            IO::Die->print( $pwrite, $message );
            IO::Die->close($pwrite);
            $written = 1;
        }
        elsif ( $rbits && $rbits ne "\0" ) {
            local ( $!, $^E );
            IO::Die->read( $pread, $sent_back, 200, length $sent_back );
        }
    }

    IO::Die->close($pread);

    waitpid $pid, 0;

    #----------------------------------------------------------------------

    ok 1;    #for Test::Class - just so we assert *something*

    return;
}

sub test_socket : Tests(3) {
    my ($self) = @_;

    my $socket = IO::Handle->new();

    is( IO::Die->socket( $socket, &Socket::PF_UNIX, &Socket::SOCK_STREAM, 0 ), 1, "socket creation ok" );

    for my $domain (&Socket::AF_INET) {
        trap {
            IO::Die->socket( my $socket, $domain, &Socket::SOCK_STREAM, -1 );
        };

        cmp_deeply(
            $trap->die(),
            all(
                re($domain),
                re('SocketOpen'),
            ),
            "socket() creation failure exception is right type and contains $domain",
        );
    }

    for my $type (&Socket::SOCK_STREAM) {
        trap {
            IO::Die->socket( my $socket, &Socket::AF_INET, $type, -1 );
        };

        cmp_deeply(
            $trap->die(),
            all(
                re($type),
                re('SocketOpen'),
            ),
            "socket() creation failure exception is right type and contains $type",
        );
    }

    return;
}

sub test_CREATE_ERROR : Test(1) {
    my $self = shift;

    trap {
        IO::Die::Subclass->read( \*STDOUT, my $fail, 123 );
    };

    cmp_deeply(
        $trap->die(),
        {
            type  => 'Read',
            attrs => isa('HASH'),
        },
        '_CREATE_ERROR can override the default exception',
    ) or diag explain $trap;

    return;
}

sub test_kill_reject_multiple : Tests(1) {
    my $pid1 = fork || do { sleep 999 };
    my $pid2 = fork || do { sleep 999 };
    trap {
        IO::Die->kill( 'TERM', $pid1, $pid2 );
    };

    $trap->did_die('kill() rejected multiple PIDs');

    kill 'TERM', $pid1, $pid2;

    return;
}

sub test_kill : Tests(8) {
    my ($self) = @_;

    my $nobody_uid = ( getpwnam $self->_dummy_user() )[2];
    my $nobody_gid = ( getgrnam $self->_dummy_user() )[2];

  SKIP: {
        skip 'Need *nix OS for tests', $self->num_tests() if !$nobody_uid;
        skip 'Must be root!',          $self->num_tests() if $>;

        my $pid = fork || do { sleep 999 };

        local ( $!, $^E );

        $!  = 5;
        $^E = 5;

        my $ret = IO::Die->kill( 'TERM', $pid );

        is( $ret,    1, 'kill() returned as it should' );
        is( 0 + $!,  5, '...and it didn’t touch $!' );
        is( 0 + $^E, 5, '...and it didn’t touch $^E' );

        my $parent_pid = $$;

        pipe my $rdr, my $wtr;

        my $parasite_pid = fork || do {
            try {
                close $rdr;

                $> = $nobody_uid;
                $) = $nobody_gid;
                $< = $nobody_uid;

                $!  = 5;
                $^E = 5;

                IO::Die->kill( 'TERM', $parent_pid );
            }
            catch {
                print {$wtr} join( $/, 0 + $!, 0 + $^E, $_ );
            }
            finally {
                exit;
            };
        };

        close $wtr;
        my @res = split m<$/>, do { local $/; <$rdr> };
        close $rdr;

        waitpid $parasite_pid, 0;

        is( $res[0], 5, 'kill() doesn’t touch $! on failure' );
        is( $res[1], 5, 'kill() doesn’t touch $^E on failure' );

        like( $res[2], qr<Kill>, 'kill() as user on a root-owned process' ) or diag explain $trap;
        like( $res[2], qr<TERM>, 'the signal is in the error' );
        like( $res[2], qr<$parent_pid>, 'the PID is in the error' );
    }

    return;
}

sub test_binmode : Tests(9) {
    my ($self) = @_;

    ( my $path, my $fh ) = $self->tempfile();

    open my $rfh, '<', $path;

    open my $rfh2, '<', $path;
    close $rfh2;

    my $err;

    local ( $!, $^E );

    $!  = 5;
    $^E = 5;

    ok(
        IO::Die->binmode($rfh),
        'binmode() returns true on success',
    );

    is( 0 + $!,  5, 'binmode() success left $! alone' );
    is( 0 + $^E, 5, 'binmode() success left $^E alone' );

    local $@;
    eval {
        local $SIG{'__WARN__'} = sub { };
        IO::Die->binmode($rfh2);
    };
    $err = $@;

    like( $err, qr<Binmode>, 'error type in error' );
    like( $err, qr<:raw>,    'default layer is in error' );
    like( $err, qr<layer>,   '...and it’s called “layer”' );

    my $errstr = do { local $! = Errno::EBADF(); "$!" };
    like( $err, qr<$errstr>, '...and the error is as expected' );

    is( 0 + $!,  5, 'binmode() failure left $! alone' );
    is( 0 + $^E, 5, 'binmode() failure left $^E alone' );

    return;
}

#----------------------------------------------------------------------

sub zzzzzzz_sanity : Test(1) {
    ok 1, 'This just ensures that STDOUT has been put back.';

    return;
}

sub _overwrite_stdout {
    my ( $self, $new_stdout ) = @_;

    open my $real_stdout_fh, '>&=', fileno select;

    select $new_stdout;    ## no critic qw(ProhibitOneArgSelect)

    return $real_stdout_fh;
}

package IO::Die::Subclass;

use strict;

use parent -norequire, 'IO::Die';

sub _CREATE_ERROR {
    my ( $NS, $type, @attrs ) = @_;

    return {
        type  => $type,
        attrs => {@attrs},
    };
}

1;
