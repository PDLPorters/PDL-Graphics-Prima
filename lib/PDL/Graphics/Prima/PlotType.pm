use strict;
use warnings;

package PDL::Graphics::Prima::PlotType;

use Carp 'croak';

=pod

To write your own plot type, you must create a class that is derived from
C<PDL::Graphics::Prima::PlotType>. (To make the discussion a bit more concrete, I
am going to use the ficticious FooBars plotType, which I suppose would plot some
fancy error bars.) Such a derived class would probably start out with these
lines of code:

 package PDL::Graphics::Prima::PlotType::FooBars;
 use base 'PDL::Graphics::Prima::PlotType';

You must then write a custom C<draw> function, and you can optionally overload
the following functions: C<xmin>, C<xmax>, C<ymin>, C<ymax>, C<initialize>.

You should also install a constructor under C<pt::FooBars> that looks like
this:

 sub pt::FooBars {
     PDL::Graphics::Prima::PlotType::FooBars->new(@_);
 }

That uses the inherited C<PDL::Graphics::Prima::PlotType::new> function, which will
eventually call your class's C<initialize> function. If your initializer expects
custom arguments, you should overload the C<initialize> function like so:

 # still in the PDL::Graphics::Prima::PlotType::FooBars package
 sub initialize {
     my ($self, @args) = @_;
     
     # You could pull items out of @args at this point if you
     # want. To call the superclass initialization do this:
     $self->SUPER::initialize(@args);
     
     # Here's some custom args processing. If the user did
     # not specify a curviness, default to 4:
     $self->{curviness} //= 4;
     
     # Could also check that the supplied values make sense:
     croak('Curviness must be a positive integer')
         unless $self->{curviness} =~ /^\d+$/
           and $self->{curviness} > 0;
 }

You could shove all of that construction functionality into C<pt::FooBars>, but
then other classes would not be able to derive functionality from your 
(undoubtedly elegant) class without resorting to rather inelegant code.

Which brings me to writing plotTypes that are derived from other plotTypes.
That is allowed, of course, in which case you can override whichever class
functions you want. At that point, you are doing normal Perl OO programming,
so it's as easy (and/or annoying) as that.

=cut

# The plotType new function. Do not override this. If you want to override how
# the plotType is constructed, override the initialize function in your derived
# class.
sub new {
	my $class = shift;
	my $self = {};
	bless $self, $class;
	$self->initialize(@_);
	return $self;
}

# Standard initialization expects key/value pairs and simply stores them in
# $self:
sub initialize {
	my $self = shift;
	croak('Standard plotTypes arguments must be in key => value pairs')
		if @_ % 2 == 1;
	my %args = @_;
	foreach my $key (keys %args) {
		$self->{$key} = $args{$key};
	}
}

=head2 xmin, xmax, ymin, ymax

These plotType functions are called when the graph needs to determine automatic
minima and maxima. Line plots simply return the data's minimum and maximum, but
more complex plot types, such as those including error bars or blobs, need to
take more details into consideration.

These functions are always called with three arguments and should always return
two values. The three arguments are:

=over

=item plotType

Either the class name for the plotType, or an actual instance of the plotType.
This passes in whatever the dataset was given for the plotType.

=item dataset

The dataset object associated with this particular plotting operation. You can
access the actual data through this object.

=item widget

The graph widget on which the data is plotted. You will have to go through this
object to get at the x- or y-axes, which contain the actual minima and maxima
of the plot.

=back

The two values it should return are:

=over

=item extremum-value

The actual minimum value needed to automatically display the data nicely. For
line plots this is just the min or max of the dataset, but if you have error
bars, for example, you would want a smaller minimum and a larger maximum to
accomodate the width or height of the error bars.

=item pixel-padding

Any extra space needed to display the results nicely. For example, the width of
blobs are specified in pixels. In that case, you would specify a minimum or
maximum value corresponding to the dataset's extremum, and specify a padding
corresponding to the blob's x- or y-radius, as appropriate.

=back

The x-version of these functions is never called by function-based datasets,
since the x-values for such datsets are determined on-the-fly. If you cannot
determine an extremum, or do not want to determine an extremum, you can return
the undefined value and it will be ignored.

=cut

sub xmin {
	my ($self, $dataset, $widget) = @_;

	# Ostensibly, this version of xmin doesn't care what invocant you called it
	# with because it does not need any information from self. It also does not
	# need any information from the widget. I unpack them here to illustrate the
	# order of the arguments.
	
	# Return the minimum of the data with a padding of 1 pixel. Note that this
	# assumes that if the data at $dataset->[0] a piddle if it is not a code
	# reference. The validation for this assumption was handled in the
	# initialize function.
	
	# This logic is made complicated by the fact that a bad value in x or y
	# should invalidate the pair. So, I resort to using the minmaxforpair
	# function, written specifically to solve this very problem.
	my ($xs, $ys) = $dataset->get_data($widget);
	
	my ($xmins) = PDL::minmaxforpair($xs, $ys);
	return ($xmins->min, 1);
}

sub xmax {
	# Get the dataset object, the second argument to this function:
	my ($dataset, $widget) = @_[1..2];
	# Get both x and y and get the xmax:
	my (undef, undef, $xmaxes) = PDL::minmaxforpair($dataset->get_data($widget));
	return ($xmaxes->max, 1);
}

sub ymin {
	my ($dataset, $widget) = @_[1..2];
	# Get both x and y and get the ymin:
	my (undef, $ymins) = PDL::minmaxforpair($dataset->get_data($widget));
	return ($ymins->min, 1);
}
sub ymax {
	my ($dataset, $widget) = @_[1..2];
	# Get both x and y and get the ymin:
	my (undef, undef, undef, $ymaxes) = PDL::minmaxforpair($dataset->get_data($widget));
	return ($ymaxes->max, 1);
}

sub draw {
	my $invocant = shift;
	my $class = ref($invocant) ? ref($invocant) : $invocant;
	croak("Plot type $class does not define its own drawing function. Please "
		. 'report this bug to the author');
}

package PDL::Graphics::Prima::PlotType::Lines;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# Install the short name constructor:
sub pt::Lines {
	PDL::Graphics::Prima::PlotType::Lines->new(@_);
}

# I don't have any special initialization to do, so I won't override it here
#sub initialize {
#	
#}

# The min/max functions work just fine for lines, but I need to define a drawing
# method:
sub draw {
	my ($self, $dataset, $widget) = @_;
	my %properties;
	# Add all of the specified polyline properties to a local collection that
	# gets passed to the polyline routine:
	foreach (@PDL::Drawing::Prima::polylines_props) {
		if (exists $self->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
		elsif (exists $dataset->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
	}

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);

	# Draw the lines:
	$widget->pdl_polylines($xs, $ys, %properties);
}

package PDL::Graphics::Prima::PlotType::Spikes;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# Install the short name constructor:
sub pt::Spikes {
	PDL::Graphics::Prima::PlotType::Spikes->new(@_);
}

# I don't have any special initialization to do, so I won't override it here
#sub initialize {
#	
#}

# The min/max functions work just fine for spikes but I need to define a drawing
# method:
sub draw {
	my ($self, $dataset, $widget) = @_;
	my %properties;
	# Add all of the specified line properties to a local collection that
	# gets passed to the line routine:
	foreach (@PDL::Drawing::Prima::lines_props) {
		if (exists $self->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
		elsif (exists $dataset->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
	}

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);
	my $zeroes = $widget->y->reals_to_pixels($ys->zeroes); #-

	# Draw the lines:
	$widget->pdl_lines($xs, $ys, $xs, $zeroes, %properties);
}



package PDL::Graphics::Prima::PlotType::Blobs;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

use Carp 'croak';
use PDL;

# Install the short name constructor:
sub pt::Blobs {
	PDL::Graphics::Prima::PlotType::Blobs->new(@_);
}

# The blobs initializer defaults to a radius of 5 pixels
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# They could have passed an xradius, a yradius, or a radius.
	my $x_radius = $self->{xRadius} // $self->{radius} // pdl(3);
	my $y_radius = $self->{yRadius} // $self->{radius} // pdl(3);
	
	# make sure the radii are piddles and croak if something goes wrong:
	eval {
		$x_radius = pdl($x_radius) unless ref($x_radius) =~ /PDL/;
		$y_radius = pdl($y_radius) unless ref($y_radius) =~ /PDL/;
		1;
	} or croak('Radii must be piddles, or values that can be interpreted by the pdl constructor');
	
	croak('Radii must be greater than or equal to 1')
		unless PDL::all($x_radius > 1) and PDL::all($y_radius > 1);
	
	# Set the internal representation of the radii to the massaged values:
	$self->{xRadius} = $x_radius;
	$self->{yRadius} = $y_radius;
}

sub xmin {
	# Get the dataset object:
	my ($self, $dataset, $widget) = @_;
	# Return the data's min and the max blob horizontal radius:
	my ($xmin) = $self->SUPER::xmin($dataset, $widget);
	return ($xmin, $self->{xRadius}->max);
}
sub xmax {
	# Get the dataset object:
	my ($self, $dataset, $widget) = @_;
	# Return the data's max and the max blob horizontal radius:
	my ($xmax) = $self->SUPER::xmax($dataset, $widget);
	return ($xmax, $self->{xRadius}->max);
}

sub ymin {
	# Get the dataset object:
	my ($self, $dataset, $widget) = @_;
	# Return the data's min and the max blob vertical radius:
	my ($ymin) = $self->SUPER::ymin($dataset, $widget);
	return ($ymin, $self->{yRadius}->max);
}
sub ymax {
	# Get the dataset object:
	my ($self, $dataset, $widget) = @_;
	# Return the data's max and the max blob horizontal radius:
	my ($ymax) = $self->SUPER::ymax($dataset, $widget);
	return ($ymax, $self->{yRadius}->max);
}

sub draw {
	my ($self, $dataset, $widget) = @_;
	my %properties;
	# Add all of the specified fill_ellipses properties to a local collection
	# that gets passed to the routine:
	foreach (@PDL::Drawing::Prima::fill_ellipses_props) {
		if (exists $self->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
		elsif (exists $dataset->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
	}
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);

	# plot it:
	$widget->pdl_fill_ellipses($xs, $ys, 2*$self->{xRadius}, 2*$self->{yRadius}
		, %properties);
}

package PDL::Graphics::Prima::PlotType::Points;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

package PDL::Graphics::Prima::PlotType::Histogram;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

use Carp 'croak';
use PDL;

# working here - does the background color make drawing a filled rectangle
# unnecessary?

# Install the short name constructor:
sub pt::Histogram {
	PDL::Graphics::Prima::PlotType::Histogram->new(@_);
}

# The histogram initializer ensures that the top padding is set to a reasonable
# value (defaults to 5 pixels):
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# Default to an upper padding of of 5 pixels:
	$self->{topPadding} //= 5;
	croak("topPadding must be a nonnegative integer")
		unless $self->{topPadding} =~ /^\d+$/ and $self->{topPadding} >= 0;
}

use PDL::NiceSlice;

# Returns user-supplied or computed bin-edge data.
# working here - apply a caching strategy?
# working here - use SUPER::xmin, et al.?
sub get_bin_edges {
	# Return the bin-edges if we have an internal copy of them:
#	return $_[0]->{binEdges} if exists $_[0]->{binEdges};
	
	my ($self, $dataset, $widget) = @_;
	
	# Compute new bin edges:
	my $xs = $dataset->get_xs($widget);
	my @dims = $xs->dims;
	$dims[0]++;
	my $widths = $xs(1,) - $xs(0,);
	my $edges = xvals(@dims) * $widths + $xs(0,);
	
	# Store these bin edges if the underlying dataset is static:
#	$self->{binEdges} = $edges unless ref($dataset) =~ /Func/;
	
	return $edges;
	
	# If the cached data is good, return it:
#	if (exists $dataset->properties->{cached_binEdges}
#		and $dataset->{cached_bin_scaling} eq $dataset->{scaling}
#	) {
#		my $edges = $dataset->properties->{cached_binEdges};
#		return $edges
#			if	$edges->nelem == $dataset->{N_points} + 1
#			and $edges->min == $widget->x->min
#			and $edges->max == $widget->x->max;
#	}
#	
#	# Otherwise, compute the bin edges, store them, and return them:
#	my $edges
#		= zeroes($dataset->{N_points} + 1)->xlinvals($widget->x->minmax);
#	$dataset->{cached_binEdges} = $edges;
#	$dataset->{cached_bin_scaling} = $dataset->{scaling};
#	return $edges;
}

sub xmin {
	# unpack the arguments:
	my ($self, $dataset, $widget) = @_;
	# Get the bin edges and return the left-most edge:
	my $edges = $self->get_bin_edges($dataset, $widget);
	# working here - used to be $edges(0)->min
	return ($edges->min, 1);
}
sub xmax {
	# unpack the arguments:
	my ($self, $dataset, $widget) = @_;
	# Get the bin edges and return the left-most edge:
	my $edges = $self->get_bin_edges($dataset, $widget);
	return ($edges->max, 1);
}

sub ymin {
	my ($self, $dataset, $widget) = @_;
	# Get the y-data:
	my $ys = $dataset->get_ys($widget);
	# Return the data's min, or zero, with no padding:
	# (This allows the histogram to include negative y-values.)
	my $ymin = $ys->min;
	$ymin = 0 if $ymin > 0;
	return ($ymin, 0);
}
sub ymax {
	my ($self, $dataset, $widget) = @_;
	# Get the y-data:
	my $ys = $dataset->get_ys($widget);
	# Return the data's max with a big padding:
	return ($ys->max, $self->{topPadding});
}

sub draw {
	my ($self, $dataset, $widget) = @_;
	my %properties;
	# Add all of the specified rectangle properties to a local collection
	# that gets passed to the routine:
	foreach (@PDL::Drawing::Prima::rectangles_props) {
		if (exists $self->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
		elsif (exists $dataset->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
	}
	
	# Get the edges and convert everything to pixels:
	my $edges = $self->get_bin_edges($dataset, $widget);
	my $pixel_edges = $widget->x->reals_to_pixels($edges);
	my $pixel_bottom = $widget->y->reals_to_pixels(0);
	my $ys = $widget->y->reals_to_pixels($dataset->get_ys($widget));
	
	$widget->pdl_rectangles($pixel_edges(0:-2), $pixel_bottom
			, $pixel_edges(1:-1), $ys, %properties);
}



package PDL::Graphics::Prima::PlotType::BoxAndWhisker;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

package PDL::Graphics::Prima::PlotType::ErrorBars;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

package PDL::Graphics::Prima::PlotType::Bands;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

1;
