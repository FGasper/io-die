use strict;
use warnings;
use autodie;

use Storable ();

use Test::More;

pipe my $rdr, my $wtr;

my $cat_pid = fork or do {
    close $rdr;

    eval "use IO::Die; 1;" or die;

    my $mod_path = $INC{'IO/Die.pm'};
    my $dir      = $mod_path;
    $dir =~ s<\.pm\z><>;

    opendir( my $dh, $dir );
    while ( my $node = readdir $dh ) {
        next if $node !~ s<\.pm\z><>;
        print {$wtr} "$node\n" or die $!;
    }
    closedir $dh;

    close $wtr;

    exit;
};

close $wtr;

my @methods = split m<\s+>, do { local $/; scalar <$rdr> };
close $rdr;

waitpid $cat_pid, 0;

plan tests => 2;

#----------------------------------------------------------------------

pipe $rdr, $wtr;

my $normal_pid = fork or do {
    close $rdr;
    eval "use IO::Die (); 1" or die;

    Storable::store_fd(
        { map { ( $_ => ref( IO::Die->can($_) ) ) } @methods },
        $wtr,
    );

    close $wtr;

    exit;
};

close $wtr;

my $from_normal = Storable::fd_retrieve($rdr);
close $rdr;

waitpid $normal_pid, 0;

is_deeply(
    $from_normal,
    { map { ( $_ => q<> ) } @methods },
    'nothing preloaded without “:preload”',
) or diag explain $from_normal;

#----------------------------------------------------------------------

pipe $rdr, $wtr;

my $preload_pid = fork or do {
    close $rdr;

    eval "use IO::Die qw(:preload); 1" or die;

    Storable::store_fd(
        { map { ( $_ => ref( IO::Die->can($_) ) ) } @methods },
        $wtr,
    );

    close $wtr;

    exit;
};

close $wtr;

my $from_preload = Storable::fd_retrieve($rdr);
close $rdr;

is_deeply(
    $from_preload,
    { map { ( $_ => 'CODE' ) } @methods },
    'everything preloaded with “:preload”',
) or diag explain $from_preload;

waitpid $preload_pid, 0;

1;
