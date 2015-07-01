package IO::Die;

use 5.006;
use strict;

#not in production
#use warnings;

=head1 NAME

IO::Die - Namespaced, error-checked I/O

=head1 VERSION

Version 0.055

=cut

our $VERSION = '0.055';

#----------------------------------------------------------------------
#PROTECTED

#Override in subclasses as needed
sub _CREATE_ERROR {
    shift;
    return shift() . ": " . join( ' ', map { defined() ? $_ : q<> } @_ );
}

sub _DO_WITH_ERROR { die $_[1] }

#----------------------------------------------------------------------
#PRIVATES

sub __THROW {
    my ( $NS, $type, @args ) = @_;

    $NS->_DO_WITH_ERROR(
        $NS->_CREATE_ERROR(
            $type,
            @args,
            OS_ERROR          => $!,
            EXTENDED_OS_ERROR => $^E,
        )
    );
}

sub __is_a_fh {
    my ($thing) = @_;

    my $is_fh;

    #Every file handle is a GLOB reference. This would be sufficient, except
    #GLOBs can also be symbol table references.
    if ( UNIVERSAL::isa( $thing, 'GLOB' ) ) {

        # You can’t tie() a symbol table reference, so if we’re tied(),
        # then this is a filehandle.
        #
        # If we’re not tied(), then we have to check fileno().
        $is_fh = ( tied *{$thing} ) || defined( CORE::fileno($thing) );
    }

    return $is_fh;
}

#----------------------------------------------------------------------

#NOTE: This function does not attempt to support every possible way of calling
#Perl’s open() built-in, but to support the minimal syntax required to do
#everything that is useful to do with open(), with preference given to those
#forms that may (somewhat arbitrarily) be considered "better".
#
#For example, this function does NOT allow one-arg or two-arg open() except for
#the more "useful" cases like when MODE is '-|' or '|-'.
#
#On the other hand, open($fh, '-') seems harder to understand than its 3-arg
#equivalent, open($fh, '<&=', STDIN), so that two-arg form is unsupported.
#
#Current forms of open() that this supports are:
#   - any form of 3 or more arguments
#   - 2-arg when the MODE is '-|' or '|-'
#
#NOTE: Bareword file handles DO NOT WORK. (Auto-vivification does, though.)
#
sub open {
    my ( $NS, $mode, $expr, @list ) = ( shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $handle_r, $mode, $expr, @list ) = ( shift, \shift, @_ );

    die "Avoid bareword file handles." if !ref $_[0] && defined $_[0] && length $_[0];
    die "Avoid one-argument open()." if !$mode;

    local ( $!, $^E );
    if ( !defined $expr ) {
        if ( $mode eq '|-' or $mode eq '-|' ) {

            #NOTE: Avoid // for compatibility with old Perl versions.
            my $open = CORE::open( $_[0], $mode );
            if ( !defined $open ) {
                $NS->__THROW('Fork');
            }

            return $open;
        }

        my $file = __FILE__;
        die "Avoid most forms of two-argument open(). (See $file and its tests for allowable forms.)";
    }

    my $ok = CORE::open( $_[0], $mode, $expr, @list ) or do {
        if ( $mode eq '|-' || $mode eq '-|' ) {
            my $cmd = $expr;

            #If the EXPR (cf. perldoc -f open) has spaces and no LIST
            #is given, then Perl interprets EXPR as a space-delimited
            #shell command, the first component of which is the actual
            #command.
            if ( !@list ) {
                ($cmd) = ( $cmd =~ m<\A(\S+)> );
            }

            $NS->__THROW( 'Exec', path => $cmd, arguments => \@list );
        }

        if ( 'SCALAR' eq ref $expr ) {
            $NS->__THROW('ScalarOpen');
        }

        $NS->__THROW( 'FileOpen', mode => $mode, path => $expr );
    };

    return $ok;
}

sub sysopen {
    my ( $NS, @post_handle_args ) = ( shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $handle_r, @post_handle_args ) = ( shift, \shift, @_ );

    my ( $path, $mode, $perms ) = @post_handle_args;

    local ( $!, $^E );

    my $ret;
    if ( @post_handle_args < 3 ) {
        $ret = CORE::sysopen( $_[0], $path, $mode );
    }
    else {
        $ret = CORE::sysopen( $_[0], $path, $mode, $perms );
    }

    #XXX: Perl bug? $! is often set here even when $ret is truthy.

    if ( !$ret ) {
        $NS->__THROW( 'FileOpen', path => $path, mode => $mode, mask => $perms );
    }

    return $ret;
}

sub chroot {
    my ( $NS, $filename ) = @_;

    local ( $!, $^E );

    if ( !defined $filename ) {
        $filename = $_;
    }

    my $ok = CORE::chroot($filename) or do {
        $NS->__THROW( 'Chroot', filename => $filename );
    };

    return $ok;
}

sub chdir {
    my ( $NS, @args ) = @_;

    local ( $!, $^E );

    my $ret;

    if (@args) {
        $ret = CORE::chdir( $args[0] ) or do {
            if ( __is_a_fh( $args[0] ) ) {
                $NS->__THROW('Chdir');
            }
            else {
                $NS->__THROW( 'Chdir', path => $args[0] );
            }
        };
    }
    else {
        $ret = CORE::chdir or do {
            my $path = _get_what_chdir_took_as_homedir();

            if ( !defined $path ) {
                $NS->__THROW('Chdir');
            }

            $NS->__THROW( 'Chdir', path => $path );
        };
    }

    return $ret;
}

sub _get_what_chdir_took_as_homedir {
    my $path = $ENV{'HOME'};
    if ( !defined $path ) {
        $path = $ENV{'LOGDIR'};

        if ( !defined($path) && $^O eq 'VMS' ) {
            $path = $ENV{'SYS$LOGIN'};
        }
    }

    return $path;
}

#A bit more restrictive than Perl’s built-in print():
#   - A file handle is still optional, but it MUST be a reference.
#
#This does still fall back to $_ and does still use the default file handle
#if either the LIST or FILEHANDLE is omitted (cf. perldoc -f print).
#
sub print {
    my ( $NS, $args_ar ) = ( shift, \@_ );

    local ( $!, $^E );

    my $ret;
    if ( __is_a_fh( $args_ar->[0] ) ) {
        $ret = CORE::print { shift @$args_ar } ( @$args_ar ? @$args_ar : $_ );
    }
    else {
        $ret = CORE::print( @$args_ar ? @$args_ar : $_ );
    }

    if ($^E) {

        #Figure out the "length" to report to the exception object.
        my $length;
        if (@$args_ar) {
            $length = 0;
            $length += length for @$args_ar;
        }
        else {
            $length = length;
        }

        $NS->__THROW( 'Write', length => $length );
    }

    return $ret;
}

sub syswrite {
    my ( $NS, $fh, @length_offset ) = ( shift, shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $fh, $buffer_sr, @length_offset ) = ( shift, shift, \shift, @_ );

    my ( $length, $offset ) = @length_offset;

    local ( $!, $^E );

    my $ret;
    if ( @length_offset > 1 ) {
        $ret = CORE::syswrite( $fh, $_[0], $length, $offset );
    }
    elsif (@length_offset) {
        $ret = CORE::syswrite( $fh, $_[0], $length );
    }
    else {
        $ret = CORE::syswrite( $fh, $_[0] );
    }

    if ( !defined $ret ) {
        my $real_length = length $_[0];

        if ($offset) {
            if ( $offset > 0 ) {
                $real_length -= $offset;
            }
            else {
                $real_length = 0 - $offset;
            }
        }

        if ( defined $length && $length < $real_length ) {
            $real_length = $length;
        }

        $NS->__THROW( 'Write', length => $real_length );
    }

    return $ret;
}

#----------------------------------------------------------------------
#NOTE: read() and sysread() implementations are exactly the same except
#for the CORE:: function call.  Alas, Perl’s prototyping stuff seems to
#make it impossible not to duplicate code here.

sub read {
    my ( $NS, $fh, @length_offset ) = ( shift, shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $fh, $buffer_sr, @length_offset ) = ( shift, shift, \shift, @_ );

    my ( $length, $offset ) = @length_offset;

    local ( $!, $^E );

    #NOTE: Perl’s prototypes can throw errors on things like:
    #(@length_offset > 1) ? $offset : ()
    #...so the following writes out the two forms of read():

    my $ret;
    if ( @length_offset > 1 ) {
        $ret = CORE::read( $fh, $_[0], $length, $offset );
    }
    else {
        $ret = CORE::read( $fh, $_[0], $length );
    }

    if ( !defined $ret ) {
        $NS->__THROW( 'Read', length => $length );
    }

    return $ret;
}

sub sysread {
    my ( $NS, $fh, @length_offset ) = ( shift, shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $fh, $buffer_sr, @length_offset ) = ( shift, shift, \shift, @_ );

    my ( $length, $offset ) = @length_offset;

    local ( $!, $^E );

    #NOTE: Perl’s prototypes can throw errors on things like:
    #(@length_offset > 1) ? $offset : ()
    #...so the following writes out the two forms of sysread():

    my $ret;
    if ( @length_offset > 1 ) {
        $ret = CORE::sysread( $fh, $_[0], $length, $offset );
    }
    else {
        $ret = CORE::sysread( $fh, $_[0], $length );
    }

    if ( !defined $ret ) {
        $NS->__THROW( 'Read', length => $length );
    }

    return $ret;
}

sub close {
    my ( $NS, $fh ) = @_;

    local ( $!, $^E );
    my $ok = ( $fh ? CORE::close($fh) : CORE::close() ) or do {
        $NS->__THROW('Close');
    };

    return $ok;
}

#NOTE: See above about read/sysread; the same duplicated code problem
#applies to seek/sysseek.

sub seek {
    my ( $NS, $fh, $pos, $whence ) = @_;

    local ( $!, $^E );
    my $ok = CORE::seek( $fh, $pos, $whence ) or do {
        $NS->__THROW( 'FileSeek', whence => $whence, position => $pos );
    };

    return $ok;
}

sub sysseek {
    my ( $NS, $fh, $pos, $whence ) = @_;

    local ( $!, $^E );
    my $ok = CORE::sysseek( $fh, $pos, $whence ) or do {
        $NS->__THROW( 'FileSeek', whence => $whence, position => $pos );
    };

    return $ok;
}

sub truncate {
    my ( $NS, $fh_or_expr, $length ) = @_;

    local ( $!, $^E );
    my $ok = CORE::truncate( $fh_or_expr, $length ) or do {
        $NS->__THROW( 'FileTruncate', length => $length );
    };

    return $ok;
}

#----------------------------------------------------------------------

sub opendir {
    my ( $NS, $dir ) = ( shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $dh_r, $dir ) = ( shift, \shift, shift );

    local ( $!, $^E );
    my $ok = CORE::opendir( $_[0], $dir ) or do {
        $NS->__THROW( 'DirectoryOpen', path => $dir );
    };

    return $ok;
}

sub rewinddir {
    my ( $NS, $dh ) = @_;

    local ( $!, $^E );
    my $ok = CORE::rewinddir($dh) or do {
        $NS->__THROW('DirectoryRewind');
    };

    return $ok;
}

sub closedir {
    my ( $NS, $dh ) = @_;

    local ( $!, $^E );
    my $ok = CORE::closedir($dh) or do {
        $NS->__THROW('DirectoryClose');
    };

    return $ok;
}

#----------------------------------------------------------------------

#NOTE: To get stat(_), do stat(\*_).
sub stat {
    my ( $NS, $path_or_fh ) = @_;

    local ( $!, $^E );

    my $ret = wantarray ? [ CORE::stat($path_or_fh) ] : CORE::stat($path_or_fh);

    if ($^E) {
        if ( __is_a_fh($path_or_fh) ) {
            $NS->__THROW('Stat');
        }

        $NS->__THROW( 'Stat', path => $path_or_fh );
    }

    return wantarray ? @$ret : $ret;
}

#NOTE: To get lstat(_), do lstat(\*_).
sub lstat {
    my ( $NS, $path_or_fh ) = @_;

    local ( $!, $^E );

    my $ret = wantarray ? [ CORE::lstat($path_or_fh) ] : CORE::lstat($path_or_fh);

    if ($^E) {
        if ( __is_a_fh($path_or_fh) ) {
            $NS->__THROW('Stat');
        }

        $NS->__THROW( 'Stat', path => $path_or_fh );
    }

    return wantarray ? @$ret : $ret;
}

#----------------------------------------------------------------------

sub fileno {
    my ( $NS, $fh ) = @_;

    local ( $!, $^E );
    my $fileno = CORE::fileno($fh);

    if ( !defined $fileno ) {
        $NS->__THROW('Fileno');
    }

    return $fileno;
}

sub flock {
    my ( $NS, $fh, $operation ) = @_;

    local ( $!, $^E );
    my $ok = CORE::flock( $fh, $operation ) or do {
        $NS->__THROW( 'Flock', operation => $operation );
    };

    return $ok;
}

#NOTE: This will only chmod() one thing at a time. It refuses to support
#multiple chmod() operations within the same call. This is in order to provide
#reliable error reporting.
#
#You, of course, can still do: IO::Die->chmod() for @items;
#
sub chmod {
    my ( $NS, $mode, $target, @too_many_args ) = @_;

    #This is here because it’s impossible to do reliable error-checking when
    #you operate on >1 filesystem node at once.
    die "Only one path at a time!" if @too_many_args;

    #NOTE: This breaks chmod’s error reporting when a file handle is passed in.
    #cf. https://rt.perl.org/Ticket/Display.html?id=122703
    local ( $!, $^E );

    my $ok = CORE::chmod( $mode, $target ) or do {
        if ( __is_a_fh($target) ) {
            $NS->__THROW( 'Chmod', permissions => $mode );
        }

        $NS->__THROW( 'Chmod', permissions => $mode, path => $target );
    };

    return $ok;
}

#NOTE: This will only chown() one thing at a time. It refuses to support
#multiple chown() operations within the same call. This is in order to provide
#reliable error reporting.
#
#You, of course, can still do: IO::Die->chown() for @items;
#
sub chown {
    my ( $NS, $uid, $gid, $target, @too_many_args ) = @_;

    #This is here because it’s impossible to do reliable error-checking when
    #you operate on >1 filesystem node at once.
    die "Only one path at a time!" if @too_many_args;

    local ( $!, $^E );

    my $ok = CORE::chown( $uid, $gid, $target ) or do {
        if ( __is_a_fh($target) ) {
            $NS->__THROW( 'Chown', uid => $uid, gid => $gid );
        }

        $NS->__THROW( 'Chown', uid => $uid, gid => $gid, path => $target );
    };

    return $ok;
}

sub link {
    my ( $NS, $old, $new ) = @_;

    local ( $!, $^E );
    my $ok = CORE::link( $old, $new ) || do {
        $NS->__THROW( 'Link', oldpath => $old, newpath => $new );
    };

    return $ok;
}

sub symlink {
    my ( $NS, $old, $new ) = @_;

    local ( $!, $^E );
    my $ok = CORE::symlink( $old, $new ) or do {
        $NS->__THROW( 'SymlinkCreate', oldpath => $old, newpath => $new );
    };

    return $ok;
}

sub readlink {
    my $NS = shift;
    my $path = @_ ? shift : $_;

    local ( $!, $^E );
    my $ok = CORE::readlink($path) or do {
        $NS->__THROW( 'SymlinkRead', path => $path );
    };

    return $ok;
}

sub rename {
    my ( $NS, $old, $new ) = @_;

    local ( $!, $^E );
    my $ok = CORE::rename( $old, $new ) or do {
        $NS->__THROW( 'Rename', oldpath => $old, newpath => $new );
    };

    return $ok;
}

#NOTE: This will only unlink() one file at a time. It refuses to support
#multiple unlink() operations within the same call. This is in order to provide
#reliable error reporting.
#
#You, of course, can still do: IO::Die->unlink() for @files;
#
sub unlink {
    my ( $NS, @paths ) = @_;

    #This is here because it’s impossible to do reliable error-checking when
    #you operate on >1 filesystem node at once.
    die "Only one path at a time!" if @paths > 1;

    if ( !@paths ) {
        @paths = ($_);
    }

    local ( $!, $^E );
    my $ok = CORE::unlink(@paths) or do {
        $NS->__THROW( 'Unlink', path => $paths[0] );
    };

    return $ok;
}

sub mkdir {
    my ( $NS, @args ) = @_;

    local ( $!, $^E );

    my $ret;
    if ( @args > 1 ) {
        $ret = CORE::mkdir $args[0], $args[1];
    }
    else {
        if ( !@args ) {
            @args = ($_);
        }

        $ret = CORE::mkdir( $args[0] );
    }

    if ( !$ret ) {
        $NS->__THROW( 'DirectoryCreate', path => $args[0], mask => $args[1] );
    }

    return $ret;
}

sub rmdir {
    my ( $NS, @args ) = @_;

    #Perl's rmdir() doesn’t actually allow batching like this,
    #but we might as well prevent anyone from trying.
    die "Only one path at a time!" if @args > 1;

    if ( !@args ) {
        @args = ($_);
    }

    local ( $!, $^E );
    my $ok = CORE::rmdir( $args[0] ) or do {
        $NS->__THROW( 'DirectoryDelete', path => $args[0] );
    };

    return $ok;
}

sub fork {
    my ($NS) = @_;

    my $pid = fork;

    $NS->__THROW('Fork') if !defined $pid;

    return $pid;
}

sub kill {
    my ( $NS, $sig, @list ) = @_;

    die "Only 1 process!" if @list > 1;

    local ( $!, $^E );
    my $ret = CORE::kill( $sig, @list );
    if ($!) {
        $NS->__THROW( 'Kill', signal => $sig, process => $list[0] );
    }

    return $ret;
}

sub exec {
    my ( $NS, $progname, @args ) = @_;

    my $ok = CORE::exec {$progname} $progname, @args or do {
        $NS->__THROW( 'Exec', program => $progname, arguments => \@args );
    };

    return $ok;
}

sub pipe {
    my ($NS) = (shift);

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $read_r, $write_r ) = ( shift, \shift, \shift );

    local ( $!, $^E );
    my $ok = CORE::pipe( $_[0], $_[1] ) or do {
        $NS->__THROW('Pipe');
    };

    return $ok;
}

my $DEFAULT_BINMODE_LAYER = ':raw';    #cf. perldoc -f binmode

sub binmode {
    my ( $NS, $fh_r, $layer ) = @_;

    if ( !defined $layer ) {
        $layer = $DEFAULT_BINMODE_LAYER;
    }

    local ( $!, $^E );
    my $ok = CORE::binmode( $fh_r, $layer ) or do {
        $NS->__THROW( 'Binmode', layer => $layer );
    };

    return $ok;
}

#NOTE: This will only utime() one thing at a time. It refuses to support
#multiple utime() operations within the same call. This is in order to provide
#reliable error reporting.
#
#You, of course, can still do: IO::Die->utime() for @items;
#
sub utime {
    my ( $NS, $atime, $mtime, $target, @too_many_args ) = @_;

    die "Only one utime() at a time!" if @too_many_args;

    local ( $!, $^E );
    my $ok = CORE::utime( $atime, $mtime, $target ) or do {
        if ( __is_a_fh($target) ) {
            $NS->__THROW( 'Utime', atime => $atime, mtime => $mtime, path => $target );
        }

        $NS->__THROW( 'Utime', atime => $atime, mtime => $mtime );
    };

    return $ok;
}

sub fcntl {
    my ( $NS, $fh, $func, $scalar ) = @_;

    local ( $!, $^E );
    my $ok = CORE::fcntl( $fh, $func, $scalar ) or do {
        $NS->__THROW( 'Fcntl', function => $func, scalar => $scalar );
    };

    return $ok;
}

sub select {
    my ( $NS, $timeout ) = ( shift, $_[3] );

    #Perl::Critic says not to use one-arg select() anyway.
    die "Need four args!" if @_ < 4;

    local ( $!, $^E );
    my ( $nfound, $timeleft ) = CORE::select( $_[0], $_[1], $_[2], $timeout );

    if ($^E) {
        $NS->__THROW('Select');
    }

    return wantarray ? ( $nfound, $timeleft ) : $nfound;
}

#----------------------------------------------------------------------

sub socket {
    my ( $NS, $domain, $type, $protocol ) = ( shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $socket_r, $domain, $type, $protocol ) = ( shift, \shift, shift, shift, shift );

    local ( $!, $^E );
    my $ok = CORE::socket( $_[0], $domain, $type, $protocol ) or do {
        $NS->__THROW( 'SocketOpen', domain => $domain, type => $type, protocol => $protocol );
    };

    return $ok;
}

sub socketpair {
    my ( $NS, $domain, $type, $protocol ) = ( shift, @_[ 2 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $socket1_r, $socket2_r, $domain, $type, $protocol ) = ( \shift, \shift, shift, shift );

    local ( $!, $^E );
    my $ok = CORE::socketpair( $_[0], $_[1], $domain, $type, $protocol ) or do {
        $NS->__THROW( 'SocketPair', domain => $domain, type => $type, protocol => $protocol );
    };

    return $ok;
}

sub bind {
    my ( $NS, $socket, $name ) = @_;

    local ( $!, $^E );
    my $ok = CORE::bind( $socket, $name ) or do {
        $NS->__THROW( 'SocketBind', name => $name );
    };

    return $ok;
}

sub connect {
    my ( $NS, $socket, $name ) = @_;

    local ( $!, $^E );
    my $ok = CORE::connect( $socket, $name ) or do {
        $NS->__THROW( 'SocketConnect', name => $name );
    };

    return $ok;
}

sub accept {
    my ( $NS, $generic_socket ) = @_[ 0, 2 ];

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $new_socket, $generic_socket ) = @_;

    local ( $!, $^E );
    my $ok = CORE::accept( $_[1], $generic_socket ) or do {
        $NS->__THROW('SocketAccept');
    };

    return $ok;
}

sub getsockopt {
    my ( $NS, $socket, $level, $optname ) = @_;

    local ( $!, $^E );
    my $res = CORE::getsockopt( $socket, $level, $optname );
    if ( !defined $res ) {
        $NS->__THROW( 'SocketGetOpt', level => $level, optname => $optname );
    }

    return $res;
}

sub setsockopt {
    my ( $NS, $socket, $level, $optname, $optval ) = @_;

    local ( $!, $^E );
    my $res = CORE::setsockopt( $socket, $level, $optname, $optval );
    if ( !defined $res ) {
        $NS->__THROW( 'SocketSetOpt', level => $level, optname => $optname, optval => $optval );
    }

    return $res;
}

sub listen {
    my ( $NS, $socket, $queuesize ) = @_;

    local ( $!, $^E );
    my $ok = CORE::listen( $socket, $queuesize ) or do {
        $NS->__THROW( 'SocketListen', queuesize => $queuesize );
    };

    return $ok;
}

sub recv {
    my ( $NS, $socket, $length, $flags ) = ( shift, shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $socket, $scalar_r, $length, $flags ) = ( shift, shift, \shift, @_ );

    local ( $!, $^E );
    my $res = CORE::recv( $socket, $_[0], $length, $flags );
    if ( !defined $res ) {
        $NS->__THROW( 'SocketReceive', length => $length, flags => $flags );
    }

    return $res;
}

sub send {
    my ( $NS, $socket, $flags, $to ) = ( shift, shift, @_[ 1 .. $#_ ] );

    #https://github.com/pjcj/Devel--Cover/issues/125
    #my ( $NS, $socket, $msg_r, $flags, $to ) = ( shift, shift, \shift, @_ );

    local ( $!, $^E );
    my $res;
    if ( defined $to ) {
        $res = CORE::send( $socket, $_[0], $flags, $to );
    }
    else {
        $res = CORE::send( $socket, $_[0], $flags );
    }

    if ( !defined $res ) {
        $NS->__THROW( 'SocketSend', length => length( $_[0] ), flags => $flags );
    }

    return $res;
}

sub shutdown {
    my ( $NS, $socket, $how ) = @_;

    local ( $!, $^E );

    my $res = CORE::shutdown( $socket, $how );
    if ( !$res ) {
        die "Invalid filehandle!" if !defined $res;
        $NS->__THROW( 'SocketShutdown', how => $how );
    }

    return $res;
}

#----------------------------------------------------------------------
# CONVENIENCE
#

my $Fcntl_SEEK_CUR = 1;

#Note that, since we die() on error, this does NOT return "0 but true"
#as sysseek() does; instead it returns just a plain 0.
sub systell {
    my ( $NS, $fh ) = @_;

    #cf. perldoc -f tell
    return 0 + $NS->sysseek( $fh, 0, $Fcntl_SEEK_CUR );
}

#----------------------------------------------------------------------

__END__

=pod

=encoding utf-8

=head1 SYNOPSIS

    use IO::Die;

    #Will throw on error:
    IO::Die->open( my $fh, '<', '/path/to/file' );
    IO::Die->print( $fh, 'Some output...' );
    IO::Die->close( $fh );

    #----------------------------------------------
    #...or, perhaps more usefully:

    package MyIO;

    use parent 'IO::Die';

    sub _CREATE_ERROR {
        my ( $NS, $type, %args ) = @_;

        return MyErrorClass->new( $type, %args );
    }

    sub _DO_WITH_ERROR {
        my ( $NS, $err ) = @_;  #$err is the result of _CREATE_ERROR() above

        return warn $err;
    }

    MyIO->open( .. );   #will warn() a MyErrorClass object on error

    #----------------------------------------------
    # You can also do:

    package MyDynamicIO;

    use parent 'IO::Die';

    sub new {
        #...something that sets an internal coderef “_create_err_cr”
    }

    sub _CREATE_ERROR {
        my ( $self, $type, %args ) = @_;
        return $self->{'_create_err_cr'}->($type, %args);
    }

    MyDynamicIO->sysopen( .. );     #uses “_create_err_cr” above

=head1 DETAILS

This module wraps most of Perl’s built-in I/O functions with code that
throws exceptions if the requested operation fails.
It confers many of C<autodie>’s benefits but with some distinctions
from that module that you may appreciate:

* C<IO::Die> does not overwrite Perl built-ins.

* C<IO::Die> does not clobber globals like $! and $^E.

* C<IO::Die> does not use function prototypes and does not export:
all calls into C<IO::Die> (or subclasses) “look like” what they are.

* C<IO::Die> does not try to impose its own error handling; you can customize
both how to represent errors and what to do with them.

* C<IO::Die> seems lighter than C<autodie> in simple memory usage checks. YMMV.

For the most part, you can simply replace:

    read( ... );

...with:

    IO::Die->read( ... );

This module, though, explicitly rejects certain “unsafe” practices that Perl
built-ins still support. Neither bareword file handles nor one-/two-arg
C<open()>, for example, are supported here--partly because it’s more
complicated to implement, but also because those patterns seem best avoided
anyway. Damian Conway’s “Perl Best Practices” and the present author’s
experience largely inform discernment of the above, which is admittedly
subjective by nature.

This module also rejects use of a single Perl command to operate on multiple
files, as e.g. Perl’s C<chmod()> allows. This is because that is the only
way to have reliable error checking, which is the whole point of this module.

Finally, since this doesn’t use function prototypes, some of the syntaxes
that Perl’s built-ins support won’t work here. You’ll likely find yourself
needing more parentheses here.

The intent, though, is that no actual functionality of Perl’s built-ins
is unimplemented; you may just need to rewrite your calls a bit
to have this module perform a given operation. For example:

    open( GLOBALS_R_BAD, '>somefile' );
    IO::Die->open( my $good_fh, '>', 'somefile');

    chown( $uid, $gid, qw( file1 file2 file 3 ) );
    IO::Die->chown( $uid, $gid, $_ ) for ( qw( file1 file2 file3 ) );

    print { $wfh } 'Haha';
    IO::Die->print( $wfh, 'Haha' );

(And, yes, unlike C<autodie>, C<IO::Die> has a C<print()> function!)

Most Perl built-ins that C<autodie> overrides have corresponding functions in this module.
Some functions, however, are not implemented here by design:

* C<readline()> and C<readdir()>: Perl’s built-ins do lots of "magic" (e.g.,
considering '0' as true in a C<while()>) that would be hard to implement.

* C<system()>: This one does a lot “under the hood”, and it’s not feasible
to avoid clobbering global $? if you use it.

* C<tell()> doesn’t write to $! and so can’t be error-checked.

* C<printf()> seems not to have much advantage over combining C<print()>
and C<sprintf()>. (?)

Some functions are thus far not implemented, including C<write()>, C<ioctl()>,
C<syscall()>, semaphore functions, etc. These can be implemented as needed.

=head1 FUNCTIONS THAT DIFFER SIGNIFICANTLY FROM THEIR PERL BUILT-INS

The following are not complete descriptions of each function; rather,
this describes the B<differences> between the relevant Perl built-in and
C<IO::Die>’s wrapper of it. It is assumed that the reader is familiar
with the built-in form.

Note that NONE of the following functions support bareword filehandles.

=head2 open()

This supports all built-in forms of 3 or more arguments. It ONLY supports
the two-argument form when the second argument (i.e., the MODE) is “|-” or “-|”.

=head2 select()

Only the four-argument form is permitted.

=head2 chmod()

=head2 chown()

=head2 kill()

=head2 unlink()

=head2 utime()

Unlike Perl’s built-ins, these will only operate on one filesystem node at a time.
This restriction is necessary for reliable error reporting because Perl’s
built-ins have no way of telling us which of multiple filesystem nodes produced
the error.

=head2 exec()

This always treats the first argument as the program name, so if you do:

    IO::Die->exec('/bin/echo haha');

… that will actually attempt to execute a program named C<echo haha> in the
directory C</bin>, which probably isn’t what you wanted and will thus fail.
(In the above case, what was likely desired was:

    IO::Die->exec('/bin/echo', 'haha');

=head1 CONVENIENCE FUNCTIONS

=head2 systell( FILEHANDLE )

This function returns the unbuffered file pointer position.

=head1 FUNCTIONS THAT LARGELY MATCH THEIR RELEVANT PERL BUILT-INS

The remaining functions intend to match their corresponding Perl built-ins;
differences should be regarded as bugs to be fixed!

=head1 CUSTOM ERROR HANDLING

C<IO::Die>’s default error format is a rather primitive one that just
consists of the error parameters in a string. By default, this error is
thrown via Perl’s C<die()> built-in. If you need more
robust/flexible error handling, subclass this module, and override the
C<_CREATE_ERROR()> and/or C<_DO_WITH_ERROR> methods.

C<_DO_WITH_ERROR()> receives these parameters:

* The namespace

* The error (i.e., the error as returned by C<_CREATE_ERROR()>)

C<_CREATE_ERROR()> receives these parameters:

* The namespace.

* A name for the error type, e.g., “FileOpen”.

* A list of key/value pairs that describe the error.

The error types are proprietary to this module and listed below.

=head1 PROPRIETARY ERROR TYPES

Each error type always has the following attributes, which are the same values
as their corresponding variables as described in “perldoc perlvar”.

* OS_ERROR

* EXTENDED_OS_ERROR

Additional attributes for each type are listed below.

=over 4

=item Binmode           layer

=item Chdir             OPTIONAL: path

=item Chmod             permissions, path

=item Chown             uid, gid, path

=item Chroot            filename

=item Close

=item DirectoryClose

=item DirectoryCreate   path, mask

=item DirectoryDelete   path

=item DirectoryOpen     path

=item DirectoryRewind

=item Exec              path, arguments

=item Fcntl             function, scalar

=item FileOpen          mode, path; OPTIONAL: mask

=item Fileno

=item FileSeek          whence, position

=item FileTruncate

=item Flock             operation

=item Fork

=item Kill              signal, process

=item Link              oldpath, newpath

=item Pipe

=item Read              length

=item Rename            oldpath, newpath

=item ScalarOpen

=item Select

=item SocketAccept

=item SocketBind        name

=item SocketConnect     name

=item SocketGetOpt      level, optname

=item SocketListen      queuesize

=item SocketOpen        domain, type, protocol

=item SocketPair        domain, type, protocol

=item SocketReceive     length, flags

=item SocketSend        length, flags

=item SocketSetOpt      level, optname, optval

=item SocketShutdown    how

=item Stat              path

=item SymlinkCreate     oldpath, newpath

=item SymlinkRead       path

=item Unlink            path

=item Utime             atime, mtime; OPTIONAL: path

=item Write             length

=back

=head1 AUTHOR

Felipe Gasper, working for cPanel, Inc.

=head1 REPOSITORY

L<https://github.com/FGasper/io-die>

=head1 REPORTING BUGS

Open an issue at the GitHub URL above. Patches are welcome!

=head1 TODO

=over 4

=item * More tests.

=item * Reduce testing dependencies.

=item * Right now this B<kind> of works on Windows, but the tests use fork(),
so there all kinds of weird failures that, while they can happen in real code,
don’t really stem from this module.

=back

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc IO::Die

=head1 LICENSE AND COPYRIGHT

Copyright 2015 Felipe Gasper.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1; # End of IO::Die
