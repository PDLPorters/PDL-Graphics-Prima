use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple;
use PDL::NiceSlice;

my $x = 3 * grandom(30);
my $y = 3 * grandom(30);
my $y_err = grandom(30);
my $white = pdl(255, 255, 255)->rgb_to_color;
my $red = pdl(255, 0, 0)->rgb_to_color;
my $black = pdl(0,0,0)->rgb_to_color;

# Plots error bars and triangles at the point.
plot(
	title => 'Simple Error Bars',
	-data => [
		$x, $y,
		plotType => [
			pt::ErrorBars(y_err => $y_err, colors => $black),
			pt::Triangles(colors => $red),
		],
	],
);

# Notice that the error bars ran through the middle of the triangles in
# the last example. Here, I plot white filled triangles, then the red
# triangle outlines.
plot(
	title => '"Unfilled" Triangles',
	-data => [
		$x, $y,
		plotType => [
			pt::ErrorBars(y_err => $y_err, colors => $black),
			pt::Triangles(
				filled => 'yes',
				colors => $white,
				orientation => 'up',
			),
			pt::Triangles(colors => $red),
		],
	],
);

# I can achieve the same thing using PDL threading, which I'll use to even
# greater effect in the next exampe:
my $colors = cat($white, $red)->transpose;
plot(
	title => 'Threaded Error Bars',
	-data => [
		$x, $y,
		plotType => [
			pt::ErrorBars(y_err => $y_err, colors => $black),
			pt::Triangles(
				filled => pdl(1, 0)->transpose,
				colors => $colors,
			),
		],
	],
);

# You may notice (it's particularly obvious if you zoom out) that triangles
# plotted over each other do not blot each other out. I can get that by
# reversing the order of the threaded drawing operations for the triangles:
plot(
	title => 'Better-looking Threaded',
	-data => [
		$x->transpose, $y->transpose,
		plotType => [
			pt::ErrorBars(y_err => $y_err->transpose, colors => $black),
			pt::Triangles(
				filled => pdl(1, 0),
				colors => $colors->transpose,
			),
		],
	],
);

