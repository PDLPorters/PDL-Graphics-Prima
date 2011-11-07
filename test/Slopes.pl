use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple -sequential;
use PDL::NiceSlice;

my $x = sequence(20)/5;
my $y = $x**2;
my $slopes = 2 * $x;

# Plots error bars and triangles at the point.
plot(
	title => 'Slope of Quadratic',
	-data => [
		$x, $y,
		plotType => [
			pt::Lines,
			pt::Slopes(slopes => $slopes, lineWidths => 2, colors => cl::LightRed),
		],
	],
);

$x = sequence(100)/10;
$y = sin($x);
$slopes = cos($x);

# Plots error bars and triangles at the point.
plot(
	title => 'Slope of Sine Wave',
	-data => [
		$x, $y,
		plotType => [
			pt::Lines,
			pt::Slopes(slopes => $slopes, lineWidths => 2, colors => cl::LightRed),
		],
	],
);

