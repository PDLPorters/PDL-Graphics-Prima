use strict;
use warnings;
use blib;
use PDL;
use PDL::NiceSlice;
use Prima qw(Application);
use PDL::PrimaPoly;

my $wDisplay = Prima::MainWindow-> create(
	text    => 'PrimaPoly Test',
	onPaint => sub {
		my ( $self, $canvas) = @_;
		my $c = $canvas-> color;
		$canvas-> color( cl::White);
		$canvas-> bar( 0, 0, $canvas-> size);
		$canvas-> color( $c);
		paint( $canvas);
	},
);

sub paint
{
	my $p = $_[0];
	my @size = $p-> size;

	$p-> color( cl::LightGray);
	$p-> fill_ellipse( $size[0], $size[1], 300, 300);

	$p-> color( cl::Black);
	$p-> rectangle( 10, 10, $size[0]-10, $size[1]-10);
	$p-> font-> size( 24);
	$p-> text_out( "Print example", 200, 200);
	
	# Draw three sin curves:
	my $x = sequence(200, 3) + 100;
	my $y = sin($x / 20) * 100 + 100;
	$y(:,1) += 50;
	$y(:,2) += 100;
	prima_polyline($x, $y, $p);
	
	# Draw a dashed line:
	$y = pdl(150);
	$x = sequence(10) + 20 * yvals(10, 50);
	prima_polyline($x, $y, $p);
}

run Prima;

