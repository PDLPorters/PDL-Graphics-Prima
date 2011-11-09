use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple -sequential;

my $xs = sequence(100)/10;
my $ys = sin($xs);
plot(
	-data => [$xs, $ys],
	color => cl::LightBlue,
);

plot(
	-data => [$xs, $ys],
	color => cl::LightGreen,
);
