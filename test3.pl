use strict;
use warnings;
use PDL;
use PDL::Graphics::Prima;
use Prima qw(Application);

my $t = sequence(300) / 25;
my $y = sin($t);

my $wDisplay = Prima::MainWindow->create(
	text    => 'Graph Test',
	onPaint => sub {
		my ( $self, $canvas) = @_;

		# wipe and replot:
		$self->clear;
		$self->graph([$t, $y], {
			xmin => $self->{xmin},
			xmax => $self->{xmax},
			ymin => $self->{ymin},
			ymax => $self->{ymax},
			}
		);
	},
	backColor => cl::White,
	onCreate => sub {
		my $self = shift;
		($self->{xmin}, $self->{xmax}) = $t->minmax;
		($self->{ymin}, $self->{ymax}) = $y->minmax;
	},
	onMouseWheel => sub {
		my $self = shift;
		my (undef, $x, $y, $dir) = @_;
		my ($x_max, $y_max) = $self->size;
		if ($x > $x_max/10) {
			my ($xmin, $xmax) = ($self->{xmin}, $self->{xmax});
			my $xrange = $xmax - $xmin;
			if ($dir > 0) {
				$self->{xmin} += $xrange/10;
				$self->{xmax} -= $xrange/10;
			}
			else {
				$self->{xmin} -= $xrange/10;
				$self->{xmax} += $xrange/10;
			}
		}
		if ($y > $y_max/10) {
			my ($ymin, $ymax) = ($self->{ymin}, $self->{ymax});
			my $yrange = $ymax - $ymin;
			if ($dir > 0) {
				$self->{ymin} += $yrange/10;
				$self->{ymax} -= $yrange/10;
			}
			else {
				$self->{ymin} -= $yrange/10;
				$self->{ymax} += $yrange/10;
			}
		}
		$self->notify("Paint");
	},
	onMouseDown => sub {
		my ($self, $button, undef, $x, $y) = @_;
		$self->{clickx} = $x;
		$self->{clicky} = $y;
	},
	onMouseMove => sub {
		my ($self, undef, $x, $y) = @_;
		return unless exists $self->{clickx};

		my ($x_max_pixel, $y_max_pixel) = $self->size;
		my $dx_pixel = $x - $self->{clickx};
		my $dy_pixel = $y - $self->{clicky};
		
		# Store the new values
		$self->{clickx} = $x;
		$self->{clicky} = $y;
		
		# Rescale dx and dy to coordinates
		my $dx = $dx_pixel / $x_max_pixel * ($self->{xmax} - $self->{xmin});
		my $dy = $dy_pixel / $y_max_pixel * ($self->{ymax} - $self->{ymin});
		
		# Adjust the x and y values:
		$self->{xmax} -= $dx;
		$self->{xmin} -= $dx;
		$self->{ymin} -= $dy;
		$self->{ymax} -= $dy;
		$self->notify("Paint");
	},
	onMouseUp => sub {
		my ($self, $button, undef, $x, $y) = @_;
		delete $self->{clickx};
		delete $self->{clicky};
	}
);



run Prima;
