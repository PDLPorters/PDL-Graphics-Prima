use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

# Create a block with random points set to one:
my $size = 200;
my $values = zeroes($size, $size);
#$values = sequence(5,5);
#print "values are $values\n";
#print "rescaled values are ", ($values - $values->min) / ($values->max - $values->min), "\n";

use PDL::NiceSlice;
for (1..500) {
	my ($x, $y) = (int(rand($size)), int(rand($size)));
#	print "Setting $x, $y to 1\n";
	$values($x, $y) .= 1;
}

# Spread out the points:
use PDL::Image2D;
my $to_plot = $values->box2d($size/10, $size/10, 1);

#print "values are $values\n";
#print "to plot is $to_plot\n";

my $wDisplay = Prima::MainWindow->create(
	text    => 'Intensity Plot Test',
	size	=> [500, 500],
);

$wDisplay->insert('Plot',
	pack => { fill => 'both', expand => 1},
	-random => [[0,1], [1,2], plotType => pt::ColorGrid(colors => $values)],
#	-points => [[0,1], [1,2], plotType => pt::ColorGrid(colors => $points)],
	-intensity => [[0, 1], [0, 1], plotType => pt::ColorGrid(colors => $to_plot
		, palette => pal::WhiteToHSV(0, 1, 1))],
	
);

run Prima;

