use strict;
use warnings;
use Module::Build;

my $build = Module::Build->new(
	module_name => 'PDL::Graphics::Prima',
	dist_version_from => 'lib/PDL/Graphics/Prima.pm',
	license  => 'perl',
	requires => {
		# I need 5.10 for defined-or. This ought to be removed and relaxed
		# after the initial release:
		perl					=> '5.10.0',
		'PDL::Drawing::Prima'	=> 0.01,
		PDL						=> '2.4.0',
	},
);

$build->create_build_script;
