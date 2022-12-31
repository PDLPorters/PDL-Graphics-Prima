use strict;
use warnings;
use Module::Build;

my $build = Module::Build->new(
	module_name => 'PDL::Graphics::Prima',
	dist_version_from => 'lib/PDL/Graphics/Prima.pm',
	license  => 'perl',
	configure_requires => {
		# meta_merge was added in v 0.28:
		'Module::Build' => 0.28,
	},
	requires => {
		# I want defined-or-equals. I don't think I need to support 5.8 anymore
		perl					=> '5.10.0',
		'PDL::Drawing::Prima'	=> 0.07,
		PDL						=> '2.4.0',
		Prima					=> 1.59,
		# Needed for event_loop
#		'Term::ReadLine'       => '1.09',
	},
	meta_merge => {
		resources => {
			repository
				=> 'http://github.com/PDLPorters/PDL-Graphics-Prima',
#				=> {
#					web => 'http://github.com/PDLPorters/PDL-Graphics-Prima',
#					url => 'git://github.com/PDLPorters/PDL-Graphics-Prima.git',
#					type => 'git',
#				},
			bugtracker
				=> 'http://github.com/PDLPorters/PDL-Graphics-Prima/issues',
#				=> {
#					web => 'http://github.com/PDLPorters/PDL-Graphics-Prima/issues',
#				},
		},
	},
);

$build->create_build_script;
