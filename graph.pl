use strict;
use warnings;
use Carp 'croak';
use Prima qw(Application);
use blib;
use PDL::Graphics::Prima;
use PDL;
use PDL::NiceSlice;
use PDL::Char;

my $wDisplay = Prima::MainWindow-> create(
	text    => 'PrimaPoly Test',
	onPaint => sub {
		my ( $self, $canvas) = @_;
		
		# Build some data to plot:
		my $x = zeroes(200)->xlinvals(31.1, 42);
		my $ys = cat(sin($x), cos($x));
		my $patterns = 	PDL::Char->new([lp::Solid, lp::Dash]);
		my $colors = pdl(cl::Blue, cl::Green);

		# Clear the canvas and draw the graph:
		$canvas->clear;
		$canvas->graph(
			  [$x, $ys, linePatterns => $patterns, colors => $colors
					, lineWidths => pdl(2)]
			, {ymin => -1, ymax => 1, xlabel => 'x', ylabel => 'y'}
		);
	},
	backColor => cl::White,
	
);

run Prima;
