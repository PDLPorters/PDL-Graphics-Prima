# This is meant to produce the pathological effects with regard to 
# autoscaling that precipitated my revamp of the autoscaling code.
# To see this behave poorly, check out commit ******

use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

my $wDisplay = Prima::MainWindow->create(
	text    => 'Pathological Auto-scaling',
	size	=> [500, 500],
);

$wDisplay->insert('Plot',
	-data => [
		pdl(1, 50), pdl(0,0),
		plotType => pt::Blobs(radius => pdl(5, 100))
		],
	pack => { fill => 'both', expand => 1},
);

run Prima;


