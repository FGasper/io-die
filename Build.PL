use 5.006;
use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'IO::Die',
    license             => 'artistic_2',
    dist_author         => q{Felipe Gasper <felipe@cpanel.net>},
    dist_version_from   => 'lib/IO/Die.pm',
    release_status      => 'stable',
    configure_requires => {
        'Module::Build' => 0,
    },
    build_requires => {
        'Capture::Tiny' => '0.30',
        'Test::Class' => '0.41',
        'Test::Deep'  => '0.01',
        'Test::NoWarnings' => '0.01',
        'Test::Trap'  => '0.3.2',
    },
    requires => {
        perl => '5.006',
    },
    add_to_cleanup     => [ 'IO-Die-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
