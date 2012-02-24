# A test/example of using trendlines

use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple;

my $xs = sequence(100)/10 - 6;
my $ys = 3 - 2 * $xs + $xs->grandom;
my $colors = pal::Rainbow->apply($xs);
plot(
	-data => ds::Pair($xs, $ys,
		plotTypes => [
			ppair::Blobs,
			ppair::TrendLines(thread_like => 'points'),
		],
		colors => $colors,
	),
);
