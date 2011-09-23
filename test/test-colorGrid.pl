use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

my @dims = (200, 150);
my $hsv = ones(3, @dims);
$hsv(0;-) .= rvals(@dims, {Center => [$dims[0] / 2, $dims[0] / 2]});
my $colors = $hsv->hsv_to_rgb->rgb_to_color;
#my $rgb = yvals(3, 256, 256);
#print "rgb(:, 20, 20) is ", $rgb(:, 20, 20), "\n";
#my $colors = $rgb->rgb_to_color;

print "colors dims are ", join (', ', $colors->dims), "\n";

my $wDisplay = Prima::MainWindow->create(
	text    => 'ColorGrid Test',
	size	=> [500, 500],
);

# Make some larger steps where x and y vary in value
my $rgb = zeroes(3, 2, 2);
$rgb(0,0,0) .= 255;
$rgb(1,1,1) .= 255;
$rgb(2,0,1) .= 255;
my $funny_colors = $rgb->rgb_to_color;
my $xs = sequence(3)**2 + 3;
my $ys = $xs**2;

$wDisplay->insert('Plot',
	-spot => [[0, 1], [0, 1], plotType => pt::ColorGrid(colors => $colors)],
#	-funny => [$xs, $ys, plotType => pt::ColorGrid(colors => $funny_colors)],
	pack => { fill => 'both', expand => 1},
#	y => {scaling => sc::Log},
);

run Prima;

