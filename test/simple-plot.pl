use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple -sequential;

my $xs = sequence(100)/10;
my $ys = sin($xs) + 3 * sequence(3)->transpose;
my $colors = pal::Rainbow->apply(sequence(3)->transpose);
plot(
	-data => [$xs, $ys,
		plotType => [
			pt::Blobs,
			pt::Lines(thread_like => 'points'),
		],
		colors => $colors,
	],
);

plot(
	-data => [$xs, $ys],
	color => cl::LightGreen,
);
