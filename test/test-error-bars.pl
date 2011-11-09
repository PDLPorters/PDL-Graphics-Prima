use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

my $wDisplay = Prima::MainWindow->create(
	text    => 'Error Bar Test',
	size	=> [500, 500],
);

$wDisplay->insert('Plot',
	-data => [
		3 * grandom(30), 3 * grandom(30),
		plotType => [
			pt::ErrorBars(y_err => grandom(30), colors => cl::LightRed,
				x_left_err => grandom(30), x_err => grandom(30)),
			pt::Blobs(radius => 4),
		]
	],
	pack => { fill => 'both', expand => 1},
	x => {
		label => 'x data',
	},
	y => {
		label => 'y data',
	},
	title => 'test',
);

run Prima;

