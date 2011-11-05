use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

my $wDisplay = Prima::MainWindow->create(
	text    => 'NGon Test',
	size	=> [500, 500],
);

$wDisplay->insert('Plot',
	-data => [
		3 * grandom(30), 3 * grandom(30),
		plotType => pt::NGons(
			orientation => 360 * random(30),
			filled => (random(30) > 0.5),
			N_points => 3 + 3 * random(30),
			
		),
	],
	pack => { fill => 'both', expand => 1},
);

run Prima;

