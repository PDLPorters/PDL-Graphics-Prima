use strict;
use warnings;
use PDL::NiceSlice;

# Note: The base class is defined near the bottom of the file because I wanted
# to write the documentation for creating new derived classes in the same space
# as where I wrote the code for the base class.

=head1 NAME

PDL::Graphics::Prima::PlotType - a collection of plot types

=head1 SYNOPSIS

 use PDL;
 use PDL::Graphics::Prima::Simple -sequential;
 my $x = sequence(100)/10;
 my $y = sin($x);
 
 # A lines+diamonds plot
 plot(
     -data => ds::Pair(
         $x,
         $y,
         plotTypes => [
             ppair::Lines,
             ppair::Diamonds,
         ],
     ),
 );
 
 # Dandelions:
 $x = random(10);
 $y = random(10) + 0.5;
 plot(
     -data => ds::Pair(
         $x,
         $y,
         plotTypes => [
             ppair::Spikes(colors => cl::Green, lineWidths => 2),
             ppair::Asterisks(N_points => 11, colors => cl::White),
         ],
     ),
     backColor => cl::LightBlue,
     color => cl::White,
 );
 

=head1 DESCRIPTION

This module provides a number of basic plot types that you can use in a
PDL::Graphics::Prima plot. As far as PDL::Graphics::Prima is concerned, there
are different kinds of data that you might want to visualize, each with their
own distinct plot types. The three kinds of basic data sets, as described in
L<PDL::Graphics::Prima::DataSet>, are Sets, Pairs, and Grids. Here, I will
discuss the different plot types that you can apply to each of these sorts of
data.

Just as you can specify properties for C<DataSet>s, you can also specify properties
for individual <PlotType>s. XXX working here - discuss PlotType-wide properties
(singular) and threaded (plural) properties, and ensure that they are handled
as documented.

 # Specify the color for each blob:
 ppair::Blobs(colors => $my_colors)
 
 # Specify different line widths for each column in the histogram:
 ppair::Histogram(lineWidths => $the_widths)

=cut


###############################################################################
#                        Distribution-based Plot Types                        #
###############################################################################

package PDL::Graphics::Prima::PlotType::Set;
use base 'PDL::Graphics::Prima::PlotType';

=head2 Sets

A set of data is an unordered collection. Usually the goal is to get some
insights into the distribution of the data. For example, if you have a
collection of the heights of students in a class, you would expect them to be
distributed normally. You could visualize that by plotting a histogram of the
heights and comparing that to a bell curve, for example. The plot types
associated with sets are C<pset::CDF> and C<pset::Hist>.

=over

=cut

############################################
# PDL::Graphics::Prima::PlotType::Set::CDF #
############################################

=item pset::CDF

This plots the cumulative distribution function for a set. Given an unordered
set of data (as a piddle), it plots a continuous line 

 increasing => 1
 normalized => 1
 offset => not yet implemented

=cut

package PDL::Graphics::Prima::PlotType::Set::CDF;
use base 'PDL::Graphics::Prima::PlotType::Set';

# Install the short name constructor:
sub pset::CDF {
	PDL::Graphics::Prima::PlotType::Set::CDF->new(@_);
}

# I decided to include the 'normalized' and 'increasing' properties.
sub initialize {
	my $self = shift;

	# Call the superclass initialization:
	$self->SUPER::initialize(@_);
	
	# Give a default values. Note that I do not need to validate these
	# because they simply need to be evaluable in boolean context. It would
	# only be a problem if they used a piddle for these:
	$self->{normalized} = 1 unless defined $self->{normalized};
	$self->{increasing} = 1 unless defined $self->{increasing};
	# If I use an offset, I need to modify the y-collation
}

# Collation is a little tricky and has different behavior between the x-
# and y- calcuations:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties = $self->generate_properties(
		@PDL::Drawing::Prima::polylines_props);
	
	# Extract the line widths, against which we'll collate:
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	# get the rest of the piddles; we don't need their names and order is
	# not important as this is for bad value checking:
	my @prop_piddles = values %properties;
	
	# Get the data:
	my $set = $self->dataset->get_data;
	
	# The y- and x-data handling are quite different:
	if ($axis_name eq 'y') {
		# Compute the y-min, either 0 or bad:
		my $ymin = $set->ngoodover == 0;
		$ymin = $ymin->setbadif($ymin == 1);
		# Compute the y-max, which is either 1, the number of good elements,
		# or bad:
		my $ymax = $set->ngoodover;
		$ymax = $ymax->setbadif($ymax == 0);
		$ymax->where($ymax > 0) .= 1 if $self->{normalized};
		
		return PDL::collate_min_max_wrt_many($ymin, $lineWidths
				, $ymax, $lineWidths, $pixel_extent, @prop_piddles);
	}
	
	# Arrived here means we are dealing with x-data:
	my ($xmin, $xmax) = $set->minmaximum;
	
	# Collate and return:
	return PDL::collate_min_max_wrt_many($xmin, $lineWidths, $xmax
		, $lineWidths, $pixel_extent, @prop_piddles);
}


# This draws the CDFS:
sub draw {
	my ($self, $canvas) = @_;
	
	my %properties = $self->generate_properties(
		@PDL::Drawing::Prima::polylines_props);
	
	# Retrieve the data from the dataset and sort:
	my $xs = $self->dataset->get_data->qsort;
	# Compute the associated heights:
	my $ys = $xs->xlinvals;
	$ys -= $xs->ngoodover->dummy(0) if not $self->{increasing};
	$ys /= $xs->maximum->dummy(0) if $self->{normalized};
	
	# Convert these xs and ys to pixels:
	$xs = $self->widget->x->reals_to_pixels($xs);
	$ys = $self->widget->y->reals_to_pixels($ys);
	
	# Draw the curves:
	$canvas->pdl_polylines($xs, $ys, %properties);
}


#############################################
# PDL::Graphics::Prima::PlotType::Set::Hist #
#############################################

=item pset::Hist

working here

=cut

=back


=cut

###############################################################################
#                            Pair-based Plot Types                            #
###############################################################################

package PDL::Graphics::Prima::PlotType::Pair;
use base 'PDL::Graphics::Prima::PlotType';

# A function that gets the data and performs the real-to-pixel conversion:
sub get_data_as_pixels {
	my $self = shift;
	my ($xs, $ys) = $self->get_data;
	
	return ($self->widget->x->reals_to_pixels($xs)
		, $self->widget->y->reals_to_pixels($ys));
}


=head2 Pairs

Many plots are based on plotting x/y pairs 

working here

of data or lines, or perhaps shaded
areas. If you think of your data as a function of a single variable, like a
time series, you will likely use these plot types to visualize your data.

=over

=cut

###################################################
# PDL::Graphics::Prima::PlotType::Pair::Lines #
###################################################

=item ppair::Lines

=for ref

 ppair::Lines( [thread_like => STRING,] options )

Draws the x/y data as lines, connecting each pair of points with a line
segment. The behavior of the line drawing depends on what kind of threading
you want. You can specify that the threading behave like lines:

 ppair::Lines(thread_like => 'lines', ...)

which is the default, or like points:

 ppair::Lines(thread_like => 'points', ...)

Threading like lines does not play well with the many point-based plotTypes.
For all of those plotTypes, you can specify one property per point (like
C<colors> and C<lineWidths>), but doing so could lead to thread index
mismatch and an error in C<collate_min_max_wrt_many>:

 Index mismatch in collate_min_max_wrt_many ...

So, if you want a line with continually changing thicknesses, or continually
changing colors, you should specify that it thread like C<points>.

However, threading like points has one major drawback, which is that it does
not properly handle line styles. For example, if you wanted a dashed curve,
you would specify

 ppair::Lines(..., lineStyles => lp::Dash)

When you thread like points, each line segment is treated as a seperate line.
That mis-applies your dashing style. For large datasets (more than a million
points), another problem with point-like threading is that it uses more
memory and CPU to perform the drawing.

=cut

package PDL::Graphics::Prima::PlotType::Pair::Lines;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

# Install the short name constructor:
sub ppair::Lines {
	PDL::Graphics::Prima::PlotType::Pair::Lines->new(@_);
}

# Handle the 'thread_over' property.
sub initialize {
	my $self = shift;

	# Call the superclass initialization:
	$self->SUPER::initialize(@_);
	
	$self->{thread_like} = 'lines' unless defined $self->{thread_like};
	$self->{thread_like} = lc $self->{thread_like};
	
	# Make sure it's a valid option:
	croak("thread_like should either be lines or points")
		unless $self->{thread_like} =~ /^(lines|points)$/;
}

# Collation needs some work, since the threading doesn't work quite right
# out-of-the-box. The line data is polyline data and it must be reduced
# first.
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	# Get the list of properties for which we need to look for bad values:
	my @prop_list = $self->{thread_like} eq 'lines'	? @PDL::Drawing::Prima::polylines_props
														: @PDL::Drawing::Prima::lines_props;
	my %properties = $self->generate_properties(@prop_list);
	
	# Extract the line widths, against which we'll collate:
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	# get the rest of the piddles; we don't need their names:
	my @prop_piddles = values %properties;
	
	# Get the data:
	my ($xs, $ys) = $self->get_data;
	my ($min_x, $min_y, $max_x, $max_y) = PDL::minmaxforpair($xs, $ys);
	
	# working here - now that minmaxforpair does not return infs, make sure
	# this works
	my ($min_to_check, $max_to_check) = ($min_x, $max_x);
	($min_to_check, $max_to_check) = ($min_y, $max_y) if $axis_name eq 'y';
	
	# Collate the min and the max:
	return PDL::collate_min_max_wrt_many($min_to_check, $lineWidths,
			$max_to_check, $lineWidths, $pixel_extent, @prop_piddles);
}

sub draw {
	my ($self, $canvas) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my @prop_list = $self->{thread_like} eq 'lines'	? @PDL::Drawing::Prima::polylines_props
														: @PDL::Drawing::Prima::lines_props;
	my %properties = $self->generate_properties(@prop_list);

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->get_data_as_pixels;
	
	if ($self->{thread_like} eq 'points') {
		# Draw from the points to their half-way points:
		my $left_xs = $xs->copy;
		my $right_xs = $xs->copy;
		$right_xs(0:-2) .= $left_xs(1:-1) .= ($xs(1:-1) + $xs(0:-2)) / 2;
		
		my $left_ys = $ys->copy;
		my $right_ys = $ys->copy;
		$right_ys(0:-2) .= $left_ys(1:-1) .= ($ys(1:-1) + $ys(0:-2)) / 2;
		$canvas->pdl_lines($left_xs, $left_ys, $right_xs, $right_ys, %properties);
	}
	else {
		# Draw the lines as single curves:
		$canvas->pdl_polylines($xs, $ys, %properties);
	}
}

####################################################
# PDL::Graphics::Prima::PlotType::Pair::Trendlines #
####################################################

=item ppair::Trendlines

=for ref

 ppair::Trendlines( [[thread_like => STRING,] [weights => PDL,]
                 [along_dim => INTEGER,] options )

Draws linear fits to the x/y data as lines. This is a descendent of
C<ppair::Lines>, so you can specify the style of threading you want employed.
You can also specify the weights that you want used for your fitting. The
default is equal weights.

If you are using multidimensional data, the fit is performed along the first
dimension by default. However, if you need to perform the fit along some
other dimension, you can specify that with the C<along_dim> key.

=cut

package PDL::Graphics::Prima::PlotType::Pair::TrendLines;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair::Lines);
use strict;
use warnings;
use PDL;

# Install the short name constructor:
sub ppair::TrendLines {
	PDL::Graphics::Prima::PlotType::Pair::TrendLines->new(@_);
}

# Allow the user to specify fit weights:
sub initialize {
	my $self = shift;

	# Call the superclass initialization:
	$self->SUPER::initialize(@_);
	
	$self->{weights} = 1 unless defined $self->{weights};
	
}

sub get_data {
	my $self = shift;
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->dataset->get_data;
	my $weights = $self->{weights};
	
	# Recompute the $ys as the fit values:
	# working here - this seems to be erroneous and it blows up when 
	# S_x is close to zero
	my $S = sumover($xs->ones/$weights);
	my $S_x = sumover($xs/$weights);
	my $S_y = sumover($ys/$weights);
	my $S_xx = sumover($xs*$xs/$weights);
	my $S_xy = sumover($xs*$ys/$weights);
	my $slope = ($S_xy * $S - $S_x * $S_y) / ($S_xx * $S - $S_x * $S_x);
	my $y0 = ($S_xy - $slope * $S_xx) / $S_x;
	
	# Store these values in case the user wants to retrieve them
	# (I need to create a method for this, and a means for getting at the
	# plotType object)
	$self->{slope} = $slope;
	$self->{intercept} = $y0;
	
	# make a new set of ys that are the linear fits:
	$ys = $y0 + $slope * $xs;
	
	return ($xs, $ys);
}

################################################
# PDL::Graphics::Prima::PlotType::Pair::Spikes #
################################################
# working here - get rid of the class-specific padding; if anything, such
# padding should be part of the general class, not this specific one

=item ppair::Spikes

=for ref

 ppair::Spikes( [x_baseline | y_baseline => PDL], options )

Draws x/y data as a collection of vertical or horizontal lines. In the default
behavior, for each (x, y) data point, it draws a line from (x, 0) to (x, y). You
can change the baseline by specifying either the C<y_baseline> or C<x_baseline>
key. For example, if you specify C<< y_baseline => 5 >>, this will draw lines
starting from (x, 5) instead of (x, 0). Specifying C<< x_baseline => -2 >> will
lead to horizontal lines instead of vertical lines, drawn from (-2, y) to
(x, y). Finally, if you specify the undefined value, as
C<< x_baseline => undef >> or C<< x_baseline => undef >>, the baseline will be
taken as the minimum of the dataset's x or y data, respectively.

=cut

package PDL::Graphics::Prima::PlotType::Pair::Spikes;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

# Install the short name constructor:
sub ppair::Spikes {
	PDL::Graphics::Prima::PlotType::Pair::Spikes->new(@_);
}

# Set padding options:
sub initialize {
	my $self = shift;

	# Call the superclass initialization:
	$self->SUPER::initialize(@_);

# Should move this to the base class, perhaps
#	# Specify the various default padding options and validate supplied values:
#	foreach my $pad_name (map $_.'_padding', qw(left right bottom top)) {
#		if (exists $self->{$pad_name}) {
#			croak("$pad_name must be a positive integer")
#				unless $self->{$pad_name} =~ /^\d+$/ and $self->{$pad_name} > 0;
#		}
#		else {
#			$self->{$pad_name} = 10;
#		}		
#	}
	
	# Ensure that a baseline exists:
	if (not exists $self->{x_baseline} and not exists $self->{y_baseline}) {
		$self->{y_baseline} = 0;
	}
	elsif(exists $self->{x_baseline} and exists $self->{y_baseline}) {
		croak("You can only specify an x_baseline or a y_baseline, not both");
	}
	foreach (qw(x_baseline y_baseline)) {
		# Make sure they supplied a defined value:
		croak("You must supply a numeric value for the baseline")
			if exists $self->{$_} and not defined $self->{$_};
		# Ensure that value is numeric:
		$self->{$_} = $self->{$_} + 0 if exists $self->{$_};
	}
}

# The collation code needs to be written:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::lines_props);
	
	# Get the data:
	my ($to_check, $extra) = my ($xs, $ys) = $self->dataset->get_data;
	($to_check, $extra) = ($ys, $xs) if $axis_name eq 'y';
	
	# The min/max depend on whether the baseline we're using has the same
	# name as the axis we're using, or not. If the baseline has the same
	# name as the axis of interest, then the spikes are drawn in the
	# direction of the axis and the needed padding is 1. If the baseline is
	# in the other axis, then the pixel extent is the line width.
	my $baseline_name = $axis_name . '_baseline';
	if (exists $self->{$baseline_name}) {
		# get the property piddles; we don't need their names:
		my @prop_piddles = (values %properties, $extra);
		
		# Collate the min and the max:
		my ($min, $max) = PDL::collate_min_max_wrt_many($to_check, 1,
			$to_check, 1, $pixel_extent, @prop_piddles);
		
		# make sure that the minima are less than or equal to zero:
		my $baseline = $self->{$baseline_name};
		my $good_min = $min->where($min->isgood);
		$good_min->where($good_min > $baseline) .= $baseline
			if($good_min->nelem > 0);
		my $good_max = $max->where($max->isgood);
		$good_max->where($good_max < $baseline) .= $baseline
			if($good_max->nelem > 0);
		
		return ($min, $max);
	}
	else {
		# Extract the line widths, against which we'll collate:
		my $lineWidths = $properties{lineWidths};
		$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
		delete $properties{lineWidths};
		# get the rest of the piddles; we don't need their names:
		my @prop_piddles = (values %properties, $extra);
		
		# Collate and return the min and the max:
		return PDL::collate_min_max_wrt_many($to_check, $lineWidths,
				$to_check, $lineWidths, $pixel_extent, @prop_piddles);
	}
}

# Here is the method for drawing spikes:
sub draw {
	my ($self, $canvas) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::lines_props);

	# Retrieve the data from the dataset:
	my $widget = $self->widget;
	my ($xs, $ys) = $self->dataset->get_data_as_pixels($widget);
	
	# Draw the lines, either horizontal or vertical, based on the given baseline
	if (exists $self->{y_baseline}) {
		my $baseline = $ys->zeroes;
		if (defined $self->{y_baseline}) {
			$baseline = $widget->y->reals_to_pixels($baseline + $self->{y_baseline});
		}
		else {
			# working here - make threadable?
			$baseline .= $ys->min;
		}
		# Draw the lines:
		$canvas->pdl_lines($xs, $ys, $xs, $baseline, %properties);
	}
	else {
		my $baseline = $xs->zeroes;
		if (defined $self->{x_baseline}) {
			$baseline = $widget->x->reals_to_pixels($baseline + $self->{x_baseline});
		}
		else {
			# working here - make threadable?
			$baseline .= $xs->min;
		}
		# Draw the lines:
		$canvas->pdl_lines($xs, $ys, $baseline, $ys, %properties);
	}
}



###############################################
# PDL::Graphics::Prima::PlotType::Pair::Blobs #
###############################################

=item ppair::Blobs

=for ref

 ppair::Blobs( [radius => PDL], [xRadius => PDL],
            [yRadius => PDL], options )

Lets you draw filled ellipses with per-point x- and y- pixel radii. If you
specify the key C<radius>, it draws filled circles with the given radius. The
more specific keys C<xRadius> and C<yRadius> override the C<radius> key.

=cut

package PDL::Graphics::Prima::PlotType::Pair::Blobs;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

use PDL::Core ':Internal';
use Carp 'croak';
use PDL;

# Install the short name constructor:
sub ppair::Blobs {
	PDL::Graphics::Prima::PlotType::Pair::Blobs->new(@_);
}

# The blobs initializer defaults to a radius of 3 pixels
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# They could have passed an xradius, a yradius, or a radius.
	#					if this is defined...			  use it
	my $x_radius	=	defined $self->{xRadius}		? $self->{xRadius}
					:	defined $self->{radius}			? $self->{radius}
					:	pdl(3);
	my $y_radius	=	defined $self->{yRadius}		? $self->{yRadius}
					:	defined $self->{radius}			? $self->{radius}
					:	pdl(3);

#	Oh, if only I could assume 5.10  :-(
#	my $x_radius = $self->{xRadius} // $self->{radius} // pdl(3);
#	my $y_radius = $self->{yRadius} // $self->{radius} // pdl(3);
	
	# make sure the radii are piddles and croak if something goes wrong:
	eval {
		$x_radius = topdl($x_radius);
		$y_radius = topdl($y_radius);
		1;
	} or croak('Radii must be piddles, or values that can be interpreted by the pdl constructor');
	
	croak('Radii must be greater than or equal to 1')
		unless PDL::all($x_radius >= 1) and PDL::all($y_radius >= 1);
	
	# Set the internal representation of the radii to the massaged values:
	$self->{xRadius} = $x_radius;
	$self->{yRadius} = $y_radius;
}

# The collation code:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::fill_ellipses_props);
	my @extras = values %properties;
	
	# Get the data and radii:
	my ($xs, $ys) = $self->dataset->get_data;
	my ($to_check, $radii);
	if ($axis_name eq 'x') {
		$to_check = $xs;
		$radii = $self->{xRadius};
		push @extras, $ys, $self->{yRadius};
	}
	else {
		$to_check = $ys;
		$radii = $self->{yRadius};
		push @extras, $xs, $self->{xRadius};
	}
	
	# Return the collated results:
	return PDL::collate_min_max_wrt_many($to_check, $radii, $to_check, $radii
		, $pixel_extent, @extras);
}

sub draw {
	my ($self, $canvas) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::fill_ellipses_props);
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->dataset->get_data_as_pixels;

	# plot it:
	$canvas->pdl_fill_ellipses($xs, $ys, 2*$self->{xRadius}, 2*$self->{yRadius}
		, %properties);
}

#####################################################
# PDL::Graphics::Prima::PlotType::Pair::Symbols #
#####################################################

=item ppair::Symbols

=for ref

 ppair::Symbols( [size => PDL], [filled => PDL::Byte],
              [N_points => PDL::Byte], [orientation => PDL],
              [skip => PDL::Byte], options )

Lets you draw various geometric symbols, mostly based on regular polygons.
This function inspired the creation of L<PDL::Drawing::Prima/pdl_symbols>,
so you should acquaint yourself with that function's terminology if you
want to understand the meaning of the options here. There are also a number
of derived Symbol plot types, as discussed below.

For each of your symbols, you can specify the size (radius), number of
points, orientation, skip, and whether or not you want the symbol filled.
These are the allowed arguments:

=over

=item size

The symbols are drawn with a fixed size in pixels. This size is the radius
of a circle that would inscribe the symbol. The default size is 5 pixels.

=item filled

You can draw filled symbols or open symbols. Filled symbols do not have
a border. You can specify a per-symbol value of 0 or 1, or you can specify
a plotType-wide value of 0, 1, 'yes', or 'no'. The default setting is
unfilled. Note that the filling takes winding number into account, so for
example, a five-sided star (skip=2) will have a hollow center. This, perhaps,
should be changed. I'm still debating about that.

=item N_points

The number of points in your symbol. Values of zero and one are interpreted
as circles; values of 2 are interpreted as line segments; values of three or
more are interpreted as regular polygons with the specified number of
points. The number of points is an integer and must be less than 256. The
default value is 5.

=item orientation

The angle in degrees. An orientation of zero points to the right, and the
angle increases in a counterclockwise fashion. You can also use the following
descriptive (case insensitive) strings:

 up    - 90 degrees
 left  - 180 degrees
 down  - 270 degrees
 right - 0 degrees, or 360 degrees

If the orientation is not specified, the polygon will be drawn 'right'.
This means that 4gons are drawn as diamonds, not squares, and triangels will
look tilted. (But see L</Triangles> and L</Squares>.)

=item skip

The default skip is 1 and leads to normal regular polygons, like a pentagon.
However, what if you want to draw a five-pointed star instead of a pentagon?
In that case, you would specify a skip of 2. This means I<draw a shape
connecting every B<other> point>. Higher values of skip are allowed, though
I am not sure how useful they would be.

=back

=cut

package PDL::Graphics::Prima::PlotType::Pair::Symbols;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

use PDL::Core ':Internal';
use Carp 'croak';
use PDL;

# Install the short name constructor:
sub ppair::Symbols {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_);
}

sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	#					if this is defined...			  use it
	my $orientation	=	defined $self->{orientation}	? $self->{orientation}
														:	'right';
	my $filled		=	defined $self->{filled}			? $self->{filled}
														:	0;
	my $N_points	=	defined $self->{N_points}		? $self->{N_points}
														:	5;
	my $size		=	defined $self->{size}			? $self->{size}
					:	defined $self->{radius}			? $self->{radius}
														:	5;
	my $skip		=	defined $self->{skip}			? $self->{skip}
														: 1;
	
	# Replace descriptive strings with meaningful numerical values:
	unless (ref $orientation) {
		#                if string looks like...		use value...
		$orientation	= $orientation =~ /^up$/i		? 90
						: $orientation =~ /^left$/i	? 180
						: $orientation =~ /^down$/i	? 270
						: $orientation =~ /^right$/i	? 0
														: $orientation;
	}
	unless (ref $filled) {
		#        if string matches...   use value...
		$filled	= $filled =~ /^yes$/i	? 1
				: $filled =~ /^no$/i	? 0
										: $filled;
	}
	
	# Make sure everything is piddles and croak if something goes wrong:
	eval {
		$orientation = topdl($orientation);
		$filled = topdl($filled);
		$N_points = topdl($N_points);
		$size = topdl($size);
		$skip = topdl($skip);
		1;
	} or croak('Symbls arguments must be piddles, or values that can be interpreted by the pdl constructor');
	
	croak('Sizes must be greater than or equal to 1') unless PDL::all($size >= 1);
	croak('N_points must be greater than or equal to 0 and less than 256')
		unless PDL::all(($N_points >= 0) & ($N_points < 256));
	croak('filled must be either 1 or zero')
		unless PDL::all(($filled == 0) | ($filled == 1));
	croak('skip must be greater than or equal to zero') unless PDL::all($skip >= 0);
	
	# Set the internal representation of the parameters to the massaged values:
	$self->{size} = $size;
	$self->{orientation} = $orientation;
	$self->{N_points} = $N_points->byte;
	$self->{filled} = $filled->byte;
	$self->{skip} = $skip->byte;
}

# The collation code:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::symbols_props);
	my @extras = values %properties;
	
	my $size = $self->{size};
	my $to_check;
	my ($xs, $ys) = $self->dataset->get_data;
	
	# working here - make this take the orientation of each polygon into account
#	my ($min_points, $max_points);
	if ($axis_name eq 'x') {
#		# compute the min_points and max_points
		$to_check = $xs;
		push @extras, $ys;
	}
	else {
#		# compute the min_points and max_points
		$to_check = $ys;
		push @extras, $xs;
	}
	
	# Return the collated results:
	return PDL::collate_min_max_wrt_many($to_check, $size, $to_check, $size
		, $pixel_extent, @extras);
}

sub draw {
	my ($self, $canvas) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %props = $self->generate_properties(@PDL::Drawing::Prima::symbols_props);
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->dataset->get_data_as_pixels;
	$canvas->pdl_symbols($xs, $ys, $self->{N_points}
		, $self->{orientation}, $self->{filled}, $self->{size}
		, $self->{skip}, %props);
}

#############################################################
# PDL::Graphics::Prima::PlotType::Pair::Symbols Derivatives #
#############################################################

=pod

In addition, there are many nicely named derivatives of ppair::Symbols. These
give descriptive names to many common symbols and include:

=over

=item Sticks

=for ref

 ppair::Sticks( [size => PDL], [orientation => PDL], options )

C<ppair::Sticks> is a wrapper around the Symbols plotType that draws 2-point polygons,
that is, sticks. You can specify the orientation and the size; you can also specify
N_points and filled, but those will be ignored.

=cut

sub ppair::Sticks {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_, N_points => 2, filled => 'no');
}

=item Triangles

=for ref

 ppair::Triangles( [size => PDL], [filled => PDL::Byte],
                [orientation => PDL], options )

C<ppair::Triangles> is a wrapper around the Symbols plotType that draws 3-point regular
polygons. It takes the same options as Symbols, except that if you specify
N_points, it will be overridden by the value 3. Also, the default orientation
which you B<can> override, is 'up'.

=cut

sub ppair::Triangles {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(orientation => 'up', @_, N_points => 3);
}

=item Squares

=for ref

 ppair::Squares( [size => PDL], [filled => PDL::Byte], options )

C<ppair::Squares> is a wrapper around Symbols that draws 4-point regular polygon with an
orientation that makes it look like a square (instead of a diamond). You can
specify vales for N_points and orientation, but they will be ignored.

=cut

sub ppair::Squares {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_, N_points => 4, orientation => 45);
}

=item Diamonds

=for ref

 ppair::Diamonds( [size => PDL], [filled => PDL::Byte], options )

C<ppair::Diamonds> is just like Squares, but rotated by 45
degrees. Again, you can specify N_points and orientation, but those will be
ignored.

=cut

sub ppair::Diamonds {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_, N_points => 4, orientation => 0);
}

=item Stars

=for ref

 ppair::Stars( [size => PDL], [N_points => PDL::Byte],
            [orientation => PDL], options )

C<ppair::Stars> creates open or filled star shapes. These only look right when
you have five or more C<N_points>, though it will plot something with four
and fewer. The default orientation is 'up' but that can be overridden. The
C<skip> of two, however, cannot be overridden. You can also specify the fill
state and the orientation, in addition to all the other Drawable parameters,
of course.

=cut

sub ppair::Stars {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(orientation => 90, @_, skip => 2);
}

=item Asterisks

=for ref

 ppair::Asterisks( [size => PDL], [N_points => PDL::Byte],
                [orientation => PDL], options )

C<ppair::Asterisks> creates N-sided asterisks. It does this by forcing a skip
of zero that cannot be overridden. As with Stars, the default orientation is
'up' but that can be overridden. You can also specify the fill state, but
that will not be used.

=cut

sub ppair::Asterisks {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(orientation => 90, @_, skip => 0);
}

=item Xs

=for ref

 ppair::Xs( [size => PDL], options )

C<ppair::Xs> creates C<x> shaped symbols. This sets all the Symbol arguments
except the size, so although you can specify them, they will be ignored.

=cut

sub ppair::Xs {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_, N_points => 4, orientation => 45, skip => 0);
}

=item Crosses

=for ref

 ppair::Crosses( [size => PDL], options )

C<ppair::Crosses> creates cross-shaped symbols, i.e. a C<+> shape. As with Xs,
you are free to set the size, but all other Symbol options are set for you and
will be ignored if you specify them.

=cut

sub ppair::Crosses {
	PDL::Graphics::Prima::PlotType::Pair::Symbols->new(@_, N_points => 4, orientation => 0, skip => 0);
}

=back

=cut

####################################################
# PDL::Graphics::Prima::PlotType::Pair::Slopes #
####################################################
#
#=item ppair::Slopes
#
#This plot type visualizes derivatives, i.e. slopes. 
#
#=cut

#######################################################
# PDL::Graphics::Prima::PlotType::Pair::Histogram #
#######################################################

=item Histogram

=for ref

 ppair::Histogram( [binEdges => PDL], [baseline => SCALAR],
                [topPadding => SCALAR], options )

Draws a histogram with bin-centers at the data's x-values and heights at the
data's y-values. Both positive and negative y-values are allowed.

Histogram assumes linear bin spacing and simply takes the space between the
first and second x-values to be the bin width. If you are plotting a
histogram with different spacing, such as quadratic or logarithmic, you will
need to compute the spacing on your own and specify the spacing using the
binEdges key:

 ppair::Histogram(binEdges => $bin_edges)

Note that binEdges should have one more element compared with your y-data,
that is, if you have 20 heights, you'll need 21 binEdges. Unfortunately,
specifying bin edges in this way does not work very well with having a
function-based dataset.

Options for this plotType include:

=over

=item baseline

The histogram is plotted as a series of rectangles. The height of the bottom
of these rectangles is zero, but you can set a different heigh using this
key.

=item binEdges

Sets the location of the bin edges; useful if your histogram does not have
identical spacing.

=item topPadding

Histograms whose tallest column runs to the top of the graph are very
confusing. This plotType includes a little bit of padding to ensure that
the top of the highest histogram is plotted below the top axis when you use
autoscaling. The same logic is applied to negative columns if you have any.

=back

The histogram plotType works decently well, but it needs improvement. Don't
be surprised if this plotType changes in the near future. Potential areas
for improvement might be the inclusion of a Scaling property as well as
filled/unfilled specifications (as in Symbols).

=cut

package PDL::Graphics::Prima::PlotType::Pair::Histogram;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

use Carp 'croak';
use PDL;
use strict;
use warnings;

# working here - does the background color make drawing a filled rectangle
# unnecessary?

# Install the short name constructor:
sub ppair::Histogram {
	PDL::Graphics::Prima::PlotType::Pair::Histogram->new(@_);
}

# The histogram initializer ensures that the top padding is set to a reasonable
# value (defaults to 5 pixels):
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# Default to an upper padding of of 10 pixels:
	$self->{topPadding} = 10 unless defined $self->{topPadding};
	croak("topPadding must be a nonnegative integer")
		unless $self->{topPadding} =~ /^\d+$/ and $self->{topPadding} >= 0;
	
	# Make sure we have a default baseline:
	$self->{baseline} = 0 unless defined $self->{baseline};
	$self->{baseline} += 0;
}


# Returns user-supplied or computed bin-edge data.
sub get_bin_edges {
	# Return the bin-edges if we have an internal copy of them:
	return $_[0]->{binEdges} if exists $_[0]->{binEdges};
	
	my ($self) = @_;
	
	# Compute linear bin edges if none are supplied:
	my $xs = $self->dataset->get_xs;
	my @dims = $xs->dims;
	$dims[0]++;
	my $widths = $xs(1,) - $xs(0,);
	my $edges = xvals(@dims) * $widths + $xs(0,);
	# working here - croak on bad bounds?
	
	# Store these bin edges if the underlying dataset is static:
	$self->{binEdges} = $edges unless ref($self->dataset) =~ /Func/;
	
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

# The collation code:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::rectangles_props);
	
	# Extract the line widths, against which we'll collate:
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	
	my ($xs, $ys) = $self->dataset->get_data;
	# For the y min/max, get the y-data, the padding, and the baseline:
	if ($axis_name eq 'y') {
		my $top_padding = $lineWidths;
		$top_padding += $self->{topPadding} if any $ys > $self->{baseline};
		my $bottom_padding = $lineWidths;
		$bottom_padding += $self->{topPadding} if any $ys < $self->{baseline};
		return PDL::collate_min_max_wrt_many($ys, $bottom_padding
			, $ys, $top_padding, $pixel_extent
			, $xs, values %properties);
	}
	# For the x min/max, get the bin edges and collate by line width:
	else {
		my $edges = $self->get_bin_edges;
		return PDL::collate_min_max_wrt_many($edges(0:-2), $lineWidths
			, $edges(1:-1), $lineWidths, $pixel_extent
			, $ys, values %properties);
	}
}


sub draw {
	my ($self, $canvas) = @_;
	my $dataset = $self->dataset;
	my $widget = $self->widget;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::rectangles_props);
	
	# Get the edges and convert everything to pixels:
	my $edges = $self->get_bin_edges($dataset, $widget);
	my $pixel_edges = $widget->x->reals_to_pixels($edges);
	my $pixel_bottom = $widget->y->reals_to_pixels($self->{baseline});
	my $ys = $widget->y->reals_to_pixels($dataset->get_ys);
	
	$canvas->pdl_rectangles($pixel_edges(0:-2), $pixel_bottom
			, $pixel_edges(1:-1), $ys, %properties);
}

###########################################################
# PDL::Graphics::Prima::PlotType::Pair::BoxAndWhisker #
###########################################################
# Plots vertical box-and-whisker at each data point

package PDL::Graphics::Prima::PlotType::Pair::BoxAndWhisker;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

#######################################################
# PDL::Graphics::Prima::PlotType::Pair::ErrorBars #
#######################################################
# Adds error bars

package PDL::Graphics::Prima::PlotType::Pair::ErrorBars;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

# working here - ensure documentation consistency

=item ppair::ErrorBars

=for ref

 ppair::ErrorBars( [x_err => PDL], [y_err => PDL]
                  [x_left_err => PDL], [x_right_err => PDL],
                  [y_upper_err => PDL], [y_lower_err => PDL],
                  [x_err_width => PDL], [y_err_width => PDL],
                  [err_width => PDL], options );

You create an error bars plotType objet with C<ppair::ErrorBars>:

You must specify at least one sort of error bar to plot, though you can mix and
match as you wish. Each error specification must be a piddle or something that
can be converted to a piddle:

 x_err       - symmetric x error bars
 y_err       - symmetric y error bars
 x_left_err  - left x error bars
 x_right_err - right x error bars
 y_upper_err - upper y error bars
 y_lower_err - lower y error bars

Note that the more specific error bars will override the less specific ones,
so if you provide C<x_err> and C<x_left_err>, the left error bars override the
basic ones.

You can also specify the width of the error bars in pixels:

 err_width   - width of both error caps
 x_err_width - width of x-error caps
 y_err_width - width of y-error caps

Again, the more specific widths override the less specific ones.

=cut

# Install the short name constructor:
sub ppair::ErrorBars {
	PDL::Graphics::Prima::PlotType::Pair::ErrorBars->new(@_);
}

# The ErrorBars initializer figures out the x and y bar widths
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# Set the x and y widths, with a default of 10:
	$self->{x_err_width} //= $self->{err_width} // 10;
	$self->{y_err_width} //= $self->{err_width} // 10;
	
	# Set the various internal variables:
	my $bars = $self->{x_left_err} // $self->{x_err}; #/
	$self->{left_bars} = $bars->abs if defined $bars;
	$bars = $self->{x_right_err} // $self->{x_err}; #/
	$self->{right_bars} = $bars->abs if defined $bars;
	$bars = $self->{y_upper_err} // $self->{y_err}; #/
	$self->{upper_bars} = $bars->abs if defined $bars;
	$bars = $self->{y_lower_err} // $self->{y_err}; #/
	$self->{lower_bars} = $bars->abs if defined $bars;
}

sub y_bars_present {
	my $self = shift;
	return exists($self->{upper_bars}) or exists($self->{lower_bars});
}

sub x_bars_present {
	my $self = shift;
	return exists($self->{left_bars}) or exists($self->{right_bars});
}

# The collation code:
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	
	# Get the list of properties for which we need to look for bad values:
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::lines_props);
	
	# Extract the line widths, which might be the index during the collation
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	
	# This gets pretty complex. For example, consider the x-min. The
	# collation must combine the x data and their widths (the widths of the
	# error bars as well as the caps) along with x-minus-errors and the
	# linewidths.
	
	my ($xs, $ys) = $self->dataset->get_data;
	my (@mins, @maxes);
	
	# I'm going to hack through this. It could probably be cleaner, so
	# I'm marking it as working here:
	if ($axis_name eq 'x') {
		# Let's get started with left error bars
		if (exists($self->{left_bars})) {
			my ($min, $max) = PDL::collate_min_max_wrt_many(
				  $xs - $self->{left_bars}, $lineWidths, $xs, 0
				, $pixel_extent, values %properties
				, $self->{x_err_width}, $lineWidths, $ys);
			push @mins, $min;
			push @maxes, $max;
		}
		# Right error bars:
		if (exists $self->{left_bars}) {
			my ($min, $max) = PDL::collate_min_max_wrt_many(
				  $xs, 0, $xs + $self->{right_bars}, $lineWidths
				, $pixel_extent, values %properties
				, $self->{x_err_width}, $lineWidths, $ys);
			push @mins, $min;
			push @maxes, $max;
		}
		# Vertical error bars:
		if ($self->y_bars_present) {
			# Build the list of widths to be the greater of the lineWidths
			# and the y_err_width
			my $a = PDL::Core::topdl($lineWidths);
			my $b = PDL::Core::topdl($self->{y_err_width});
			($b, $a) = ($a, $b) if $a->ndims < $b->ndims;
			my $width = $a->cat($b)->mv(-1,0)->maximum;
			
			if (exists $self->{upper_bars}) {
				my ($min, $max) = PDL::collate_min_max_wrt_many(
					  $xs, $width, $xs, $width
					, $pixel_extent, values %properties
					, $self->{y_err_width}, $lineWidths, $ys
					, $self->{upper_bars});
				push @mins, $min;
				push @maxes, $max;
			}
			if (exists $self->{lower_bars}) {
				my ($min, $max) = PDL::collate_min_max_wrt_many(
					  $xs, $width, $xs, $width
					, $pixel_extent, values %properties
					, $self->{y_err_width}, $lineWidths, $ys
					, $self->{lower_bars});
				push @mins, $min;
				push @maxes, $max;
			}
		}
	}
	else {
		# Start with lower error bars
		if (exists($self->{lower_bars})) {
			my ($min, $max) = PDL::collate_min_max_wrt_many(
				  $ys - $self->{lower_bars}, $lineWidths, $ys, 0
				, $pixel_extent, values %properties
				, $self->{y_err_width}, $lineWidths, $xs);
			push @mins, $min;
			push @maxes, $max;
		}
		# Upper error bars:
		if (exists $self->{upper_bars}) {
			my ($min, $max) = PDL::collate_min_max_wrt_many(
				  $ys, 0, $ys + $self->{upper_bars}, $lineWidths
				, $pixel_extent, values %properties
				, $self->{y_err_width}, $lineWidths, $xs);
			push @mins, $min;
			push @maxes, $max;
		}
		# Vertical error bars:
		if ($self->x_bars_present) {
			# Build the list of widths to be the greater of the lineWidths
			# and the y_err_width
			my $a = PDL::Core::topdl($lineWidths);
			my $b = PDL::Core::topdl($self->{x_err_width});
			($b, $a) = ($a, $b) if $a->ndims < $b->ndims;
			my $width = $a->cat($b)->mv(-1,0)->maximum;
			
			if (exists $self->{left_bars}) {
				my ($min, $max) = PDL::collate_min_max_wrt_many(
					  $ys, $width, $ys, $width
					, $pixel_extent, values %properties
					, $self->{x_err_width}, $lineWidths, $xs
					, $self->{left_bars});
				push @mins, $min;
				push @maxes, $max;
			}
			if (exists $self->{right_bars}) {
				my ($min, $max) = PDL::collate_min_max_wrt_many(
					  $ys, $width, $ys, $width
					, $pixel_extent, values %properties
					, $self->{x_err_width}, $lineWidths, $xs
					, $self->{right_bars});
				push @mins, $min;
				push @maxes, $max;
			}
		}
	}
	
	# combine all of them
	if (@mins > 1) {
		# combine with cat and return
		return PDL::cat(@mins), PDL::cat(@maxes);
	}
	elsif (@mins = 1) {
		return (@mins, @maxes);
	}
	else {
		# return arrays full of bad values
		my $to_return = zeroes($pixel_extent+1)->setvaltobad(0);
		return ($to_return, $to_return);
	}
}

sub draw {
	my ($self, $canvas) = @_;
	my ($xs, $ys) = $self->dataset->get_data;
	my $widget = $self->widget;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::lines_props);
	
	#---( left error bars )---#
	if (exists $self->{left_bars}) {
		my $left_xs = $xs - $self->{left_bars};
		
		# Convert from points to pixels:
		$left_xs = $widget->x->reals_to_pixels($left_xs);
		my $local_xs = $widget->x->reals_to_pixels($xs);
		my $local_ys = $widget->y->reals_to_pixels($ys);

		# Draw the line from the point to the edge:
		$canvas->pdl_lines($left_xs, $local_ys, $local_xs, $local_ys, %properties);
		# Draw the end caps:
		my $width = $self->{x_err_width}/2;
		$canvas->pdl_lines($left_xs, $local_ys + $width, $left_xs
			, $local_ys - $width, %properties);
	}
	
	#---( right error bars )---#
	if (exists $self->{right_bars}) {
		my $right_xs = $xs + $self->{right_bars};
		
		# Convert from points to pixels:
		$right_xs = $widget->x->reals_to_pixels($right_xs);
		my $local_xs = $widget->x->reals_to_pixels($xs);
		my $local_ys = $widget->y->reals_to_pixels($ys); #--

		# Draw the line from the point to the edge:
		$canvas->pdl_lines($local_xs, $local_ys, $right_xs, $local_ys, %properties);
		# Draw the end caps:
		my $width = $self->{x_err_width} / 2;
		$canvas->pdl_lines($right_xs, $local_ys + $width, $right_xs
			, $local_ys - $width, %properties);
	}
	
	#---( upper error bars )---#
	if (exists $self->{upper_bars}) {
		my $upper_ys = $ys + $self->{upper_bars};
		
		# Convert from points to pixels:
		$upper_ys = $widget->y->reals_to_pixels($upper_ys); #--
		my $local_xs = $widget->x->reals_to_pixels($xs);
		my $local_ys = $widget->y->reals_to_pixels($ys); #--

		# Draw the line from the point to the edge
		$canvas->pdl_lines($local_xs, $local_ys, $local_xs, $upper_ys, %properties);
		# Draw the caps:
		my $width = $self->{y_err_width} / 2;
		$canvas->pdl_lines($local_xs - $width, $upper_ys, $local_xs + $width
			, $upper_ys, %properties);
	}
	
	#---( lower error bars )---#
	if (exists $self->{lower_bars}) {
		my $lower_ys = $ys - $self->{lower_bars};
		
		# Convert from points to pixels:
		$lower_ys = $widget->y->reals_to_pixels($lower_ys); #--
		my $local_xs = $widget->x->reals_to_pixels($xs);
		my $local_ys = $widget->y->reals_to_pixels($ys); #--

		# Draw the line from the point to the edge:
		$canvas->pdl_lines($local_xs, $local_ys, $local_xs, $lower_ys, %properties);
		# Draw the caps
		my $width = $self->{y_err_width} / 2;
		$canvas->pdl_lines($local_xs - $width, $lower_ys, $local_xs + $width
			, $lower_ys, %properties);
	}
}

###################################################
# PDL::Graphics::Prima::PlotType::Pair::Bands #
###################################################
# Plots symmetric or unsymmetric error bands around the data

package PDL::Graphics::Prima::PlotType::Pair::Bands;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);

##################################################
# PDL::Graphics::Prima::PlotType::Pair::Area #
##################################################
# Plots shaded area, where one edge is the data

package PDL::Graphics::Prima::PlotType::Pair::Area;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Pair);


#############################################################################
#                                Raster Role                                #
#############################################################################

# This provides a couple of functions that are used by two distinctly
# different classes, namely basic images and matrix grids.

package PDL::Graphics::Prima::PlotType::Role::Raster;

# Collation function is really, really simple - just figure out the edges.
# This requires that the consuming class provides a dataset that knows how
# to report its edges.
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::bars_props);

	my $to_check = $self->dataset->edges($axis_name);
	my ($left_check, $right_check, $left_extra, $right_extra);
	if ($axis_name eq 'x') {
		$left_check = $to_check(:-2);
		$right_check = $to_check(1:);
		$left_extra = $self->dataset->edges('y')->(1:)->dummy(0);
		$right_extra = $self->dataset->edges('y')->(:-2)->dummy(0);
	}
	else {
		$left_check = $to_check(:-2)->dummy(0);
		$right_check = $to_check(1:)->dummy(0);
		$left_extra = $self->dataset->edges('x')->(1:);
		$right_extra = $self->dataset->edges('x')->(:-2);
	}
	
	# Fudging a little bith with $extra, this could probably be improved
	# working here
	return PDL::collate_min_max_wrt_many($left_check, 0, $right_check, 0
		, $pixel_extent, values %properties, $left_extra, $right_extra);
}

# Drawing is fairly straight-forward. The consuming class must provide a
# get_colored_data function.
sub draw {
	my ($self, $canvas) = @_;
	my $dataset = $self->dataset;
	my $widget = $self->widget;
	
	# Convert the x and y edges to coordinate edges:
	my $xs = $widget->x->reals_to_pixels($dataset->edges('x'));
	my $ys = $widget->y->reals_to_pixels($dataset->edges('y'));
	
	# Gather the properties that I will need to use in the plotting.
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::bars_props);
	
	# Set up the x- and y- dimension lists for proper threading:
	my $colors = $self->get_colored_data;
	$xs = $xs->dummy(1, $colors->dim(1));
	$ys = $ys->dummy(0, $colors->dim(0));

	# Now draw the bars for each rectangle:
	$canvas->pdl_bars($xs(:-2), $ys(0,:-2), $xs(1:), $ys(0,1:), %properties
		, colors => $colors);
}



###############################################################################
#                            Grid-based Plot Types                            #
###############################################################################

=back

=head2 Grid-based plot types

Other plots focus on using color or greyscale to visualize data that is a
function of two variables. If you need to visualize the elements of a matrix,
you will use these plot types. If would like to
visualize an image and have already computed the RGB, HSV, or similar values,
you should use pimage::Basic instead.

=over

=cut

package PDL::Graphics::Prima::PlotType::Grid;
use base 'PDL::Graphics::Prima::PlotType';

#################################################
# PDL::Graphics::Prima::PlotType::Grid::Contour #
#################################################

#package PDL::Graphics::Prima::PlotType::Grid::Contour;
#our @ISA = qw(PDL::Graphics::Prima::PlotType::Contour);

# working here - contour plot visualization of gridded data

################################################
# PDL::Graphics::Prima::PlotType::Grid::Matrix #
################################################
# Plots a matrix with a specified palette

package PDL::Graphics::Prima::PlotType::Grid::Matrix;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Grid);

use Carp 'croak';

=item pgrid::Matrix

=for ref

 pgrid::Matrix( [palette => PDL::Graphics::Prima::Palette], options )

This plot type lets you specify colors or values on a grid to visualize
rasterized contour plots (as opposed to line contour plots).
The default palette is a greyscale one but you can specify whichever
palette you like. See L<PDL::Graphics::Prima::Palette>. If would like to
visualize an image and have already computed the RGB, HSV, or similar values,
you should use pimage::Basic instead.

The x- and y-bounds of your Grid are taken from the dataset x- and y-bounds,
so look into L<PDL::Graphics::Prima::DataSet> for details.

working here - expand, give an example

=cut

# Install the short-name constructor:
sub pgrid::Matrix {
	PDL::Graphics::Prima::PlotType::Grid::Matrix->new(@_);
}

use PDL::Graphics::Prima::Palette;

# In the initialization, check that they supplied a color grid. I'd check the
# x and y data, but I don't have access to that here:
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# make sure we have a basic palette:
	unless (exists $self->{palette}) {
		$self->{palette} = pal::WhiteToBlack;
	}
	$self->{palette}->plotType($self) if defined $self->{palette};
}

# Collation and drawing are handled by the raster role, but require the
# existence of the get_colored_data function:
*compute_collated_min_max_for
= \&PDL::Graphics::Prima::PlotType::Role::Raster::compute_collated_min_max_for;
*draw = \&PDL::Graphics::Prima::PlotType::Role::Raster::draw;

sub get_colored_data {
	my $self = shift;
	return $self->{palette}->apply($self->dataset->get_data);
}

##############################################################################
#                           Image-based Plot Types                           #
##############################################################################

package PDL::Graphics::Prima::PlotType::Image;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

=back

=head2 Image-based Plot Types

While Grid-based plots focus on imaging data with a specified palette,
Image-based plots focus on plotting data that has already been converted to
a color representation.

working here - more details

=cut

package PDL::Graphics::Prima::PlotType::Image::Basic;
our @ISA = qw(PDL::Graphics::Prima::PlotType::Image);

use Carp 'croak';

# short form constructor:
sub pimage::Basic {
	PDL::Graphics::Prima::PlotType::Image::Basic->new(@_);
}

# no need for initialize because there is nothing special to handle!
# sub initialize {
#	my $self = shift;
#	$self->SUPER::initialize(@_);
#}

# Collation and drawing are handled by the raster role, but require the
# existence of the get_colored_data function:
*compute_collated_min_max_for
= \&PDL::Graphics::Prima::PlotType::Role::Raster::compute_collated_min_max_for;
*draw = \&PDL::Graphics::Prima::PlotType::Role::Raster::draw;

sub get_colored_data {
	return $_[0]->dataset->get_prima_color_data;
}


###############################################################################
#                         Creating your own Plot Type                         #
###############################################################################
# Also includes the base type

package PDL::Graphics::Prima::PlotType;

use Carp 'croak';

=back

=head2 Creating new plot types

If the supplied plot types do not match your needs, you can always make a new
one: all of the code for all of these plot types is written in Perl, so it isn't
too difficult. This section describes how to create custom plot types for your
own needs.

To write your own plot type, you must create a class that is derived from
C<PDL::Graphics::Prima::PlotType>. (To make the discussion a bit more concrete,
I am going to use the ficticious FooBars plotType, which I suppose would plot
some fancy error bars.) Such a derived class would probably start out with these
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

That uses the inherited C<PDL::Graphics::Prima::PlotType::new> function, which
will eventually call your class's C<initialize> function. If your initializer
expects custom arguments, you should overload the C<initialize> function like
so:

 # still in the PDL::Graphics::Prima::PlotType::FooBars package
 sub initialize {
     my $self = shift;
     
     # You could pull items out of @args at this point if you
     # want. To call the superclass initialization do this:
     $self->SUPER::initialize(@_);
     
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
undoubtedly elegant class without resorting to rather inelegant code.

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

=head2 compute_collated_min_max_for

This plotType function is called when the graph needs to determine automatic
minima and maxima. It is hard to explain and will require some attention in
the future to flesh out its documentation. My apologies for now.

This function is called with three arguments and should always return
two piddles. The return values should be of the sort described in
L<PDL::Drawing::Prima::collate_min_max_wrt_many>. The three arguments are:

=over

=item plotType object (or class name)

This is whatever you created with your constructor; if you're following the
example above, that would be an instance of the plotType. This passes in
whatever the dataset was given for the plotType.

=item axis_name

The axis for which we need to know the min and the max

=item pixel_extent

The width into which the 

=back

If you cannot determine an extremum, or do not want to determine an extremum,
you can return two piddles of size C<$pixel_extent> filled with bad values.

=cut

sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::lines_props);
	
	# Extract the line widths, against which we'll collate:
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	# get the rest of the piddles; we don't need their names:
	my @prop_piddles = values %properties;
	
	# Get the data:
	my ($xs, $ys) = $self->dataset->get_data;
	my ($to_check, $extra) = ($xs, $ys);
	($to_check, $extra) = ($ys, $xs) if $axis_name eq 'y';
	
	# Collate:
	return PDL::collate_min_max_wrt_many($to_check, $lineWidths
		, $to_check, $lineWidths, $pixel_extent, $extra, @prop_piddles);
}

=head2 generate_properties

Needs to be explained. Basically, this accumulates all the properties from the
plotType object together with those from the dataset into a single hash that
you can submit to one of the (PDL-based) Prima drawing methods.

This function is provided for your use in the draw() function. You should not
usually have a need to override it.

=cut

# This function exists to aggregate the list of properties that have been set
# for the dataset in general (I want all plot types for this data to be red) and
# for this particular plotType object (I want the error bars to have a lineWidth
# of 3):
sub generate_properties {
	my ($self, @prop_list) = @_;
	my %properties;
	my $dataset = $self->dataset;
	
	# Add all of the specified properties to a local collection that eventually
	# gets passed to the low-level drawing routine:
	foreach (@prop_list) {
		if (ref($self) and exists $self->{$_}) {
			$properties{$_} = $self->{$_};
		}
		elsif (exists $dataset->{$_}) {
			$properties{$_} = $dataset->{$_};
		}
	}
	
	# Add a check for the handles property of the widget. This is a hack to
	# enable plotting to other canvases:
	if (ref ($self) and exists $self->widget->{handles}) {
		$properties{handles} = $self->widget->{handles};
	}
	
	return %properties;
}

=head2 widget

Returns the widget that owns the dataType which, in turn, called this drawing
operation.

=head2 dataset

Returns the dataSet that owns this plotType.

=head2 get_data

Shorthand: Returns the result of calling the dataset's get_data function.

=cut

sub widget {
	return $_[0]->dataset->widget;
}

sub dataset {
	$_[0]->{dataSet} = $_[1] if (@_ == 2);
	return $_[0]->{dataSet};
}

# A function that gets the data, meant to be overloaded:
sub get_data {
	return $_[0]->dataset->get_data;
}

=head2 draw

Needs explanation and examples. This function will be called whenever the
plot widget needs to redraw your plotType (window resizes, zooms, etc). It is
a simple method call, and is called with the plotType object as the first and
only argument.

Now, something that I I<always> forget to do is to convert the data values to
pixel values. You do that with the widget's x- and y-axis objects with code like

 my $x_to_plot = $self->widget->x->reals_to_pixels($xs)

If it seems like your new plot type is not plotting anything, be sure that you
have properly converted the points you are trying to plot.

=cut

#sub draw {
#	my $invocant = shift;
#	my $class = ref($invocant) ? ref($invocant) : $invocant;
#	croak("Plot type $class does not define its own drawing function. Please "
#		. 'report this bug to the author');
#}


############################################
# PDL::Graphics::Prima::PlotType::CallBack #
############################################
# Calls user-supplied callbacks for all major functions
# Provides a playground to try out new plot types

package PDL::Graphics::Prima::PlotType::CallBack;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# working here - this isn't working the way it's supposed to, and I need to
# update the documentation since I no longer have the min/max functions, but
# I do need the collation function.

=head1 CallBack

This is a cool class, but it's not working at the moment. :-(

This class lets you supply your own callbacks for auto-scaling min and max
calculations and for the drawing routines. This may seem overly high-level, but
it's mostly here so that you can implement custom drawing routines, implement
user-level tweaks to existing classes, and toy around with new plot types
without having to write a full-blown class.

=head2 New Drawing Techniques

If you like the way that a class operates but want to use your own drawing
routines, you can specify a base class and a drawing callback like so:

 my $smiley_plot_type = pt::CallBack(
 	base_class => 'PDL::Graphics::Prima::PlotType::Pair::Blobs',
 	draw => sub {
 		my ($self, $canvas) = @_;
 		
 		# Retrieve the data from the dataset:
 		my ($xs, $ys) = $self->dataset->get_data_as_pixels($widget);
 		
 		# Draw the smileys:
 		$canvas->pdl_ellipses($xs, $ys, 10, 10);	# face
 		$canvas->pdl_fill_ellipses($xs - 5, $ys + 4, 2, 2);	# left eye
 		$canvas->pdl_fill_ellipses($xs + 5, $ys + 4, 2, 2); # right eye
 		$canvas->pdl_fill_chords($xs, $ys + 3, 10, 10, 200, 340); # smiling mouth
 	},
 	radius => 10,	# be sure to coordinate with pdl_ellipses, above
 );

This will use the Blobs methods for determining xmin, xmax, ymin, and ymax, but
use this custom method for drawing smileys.

=cut


# Install the short name constructor:
sub pt::CallBack {
	PDL::Graphics::Prima::PlotType::CallBack->new(@_);
}

# Set up the callback functions:
sub initialize {
	my $self = shift;

	# Basic initialization:
	PDL::Graphics::Prima::PlotType::initialize($self, @_);
	
	# Default the base class to the most basic one:
	croak("You must supply a base class") unless $self->{base_class};
	croak('Base class must be a valid PlotType')
		unless eval{$self->{base_class}->isa('PDL::Graphics::Prima::PlotType')};
	
	# Call the superclass initialization:
	my $init = $self->{base_class}->can('initialize');
	&$init($self, @_);
	
	# Make sure that we have a valid draw function:
	unless ($self->{base_class}->can('draw')) {
		croak('You must supply a draw function or a drawable base class to the CallBack plot class')
			unless exists $self->{draw} and ref ($self->{draw})
				and ref ($self->{draw}) eq 'CODE';
		
	}
}

# Dynamic drawing based upon values of the draw key and/or the base class:
sub draw {
	my ($self, $canvas) = @_;
	
	if (not exists $self->{draw}) {
		# Didn't supply a draw function, so call the base class's draw function:
		croak('CallBack plot type must supply a drawing function or '
			. 'a base class that can draw itself.')
			unless $self->{base_class}->can('draw');
		
		# Masquerade as the base class:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->draw($canvas);
		bless $self, $class;
		return;
	}
	
	if (ref($self->{draw}) and ref($self->{draw}) eq 'CODE') {
		# They supplied a code reference, so run it:
		my $func = $self->{draw};
		&$func($self);
		return;
	}
	
	croak('You must supply a code reference for your drawing code.');
}

1;

=head1 TODO

I have lots of things that need to happen to improve this library.

=over

=item Consistencies

Are string-properties OK, or should they be constants? (threadlike, for example)
Should properties be threadable?

=item Some plot types are characteristically different

I need to split the plotTypes up based upon the nature of the data.
For example, ColorGrids fundamentally works with gridded 2d data, and
should be put into a different group from Lines, which fundamentally works
with 1d time series. This also suggests a collection of plot types for 0d data,
by which I guess I mean sets of numbers. In that situation, cumulative
distibution plots and probability distribution plots (i.e. histograms)
would be good.

here's a potential naming scheme for the different plot types that I can
think of:

 pdist::CDF - plot distribution, cumulative distribution function
 ppair::Lines - plot sequence, Lines
 pgrid::Colors - plot grid, Colors (ColorGrid)

This would then be associated with different dataSets, which would have
constructor names like:

 ds::Set
 ds::Pair
 ds::Grid

=item Add support for 3d Plots

Dmitry has written a proof-of-concept widget that uses openGL and it should
be possible to make many of these plotTypes work with 3d data just as well
as with 2d data.

=item Documentation on Combining

I need to explain how to use multiple plotTypes together in the DESCRIPTION.
(For now, the best discussion is in L<PDL::Graphics::Prima::Simple>, in case
you're looking.)

=item New Plot Types

There are many, many plot types that are not yet supported, but should
be. Sequential plot-types that come to mind include:

=over

=item arrows

Draw flow-fields with arrows of various sizes and orientations.
Args: orientation, style => head, tail (ORable?), filled, length

=item error-bands

I would really like to be able to draw error-bands around a best-fit function.

=item box-and-whisker

Box-and-whisker plots should be easy enough, a simple extension of error bars.

=back

Set plot-types that come to mind include many fitting types, such as:

 gauss-cdf-fit     gauss-pdf-fit
 lorentz-cdf-fit   lorentz-pdf-fit
 p-law-cdf-fit     p-law-pdf-fit
 log-norm-cdf-fit  log-norm-pdf-fit
 exp-cdf-fit       exp-pdf-fit
 beta-cdf-fit      beta-pdf-fit

Surely there are others. In addition:

=over

=item PairSet

A plot type that bins Pair data in x/y bins and plots a greyscale. This
would be useful for visualizing huge quantities of x/y data, when plotting
with points would fail due to too many in the same place.

=back

=item simpler image support

pgrid::Color, while immensely flexible, is very slow. Prima has hooks for adding
images to a Drawable object, but they have not yet been incorporated into
L<PDL::Drawing::Prima>. Once that happens, fast and scalable image support will
be possible.

=item consistent interface for caching

Many of these plottypes could speed up bounds calculations by caching certain
results. I need to implement a generic interface for caching, and cache clearing.

=back

=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 SEE ALSO

This is a component of L<PDL::Graphics::Prima>. This library is composed of many
modules, including:

=over

=item L<PDL::Graphics::Prima>

Defines the Plot widget for use in Prima applications

=item L<PDL::Graphics::Prima::Axis>

Specifies the behavior of axes (but not the scaling)

=item L<PDL::Graphics::Prima::DataSet>

Specifies the behavior of DataSets

=item L<PDL::Graphics::Prima::Internals>

A dumping ground for my partial documentation of some of the more complicated
stuff. It's not organized, so you probably shouldn't read it.

=item L<PDL::Graphics::Prima::Limits>

Defines the lm:: namespace

=item L<PDL::Graphics::Prima::Palette>

Specifies a collection of different color palettes

=item L<PDL::Graphics::Prima::PlotType>

Defines the different ways to visualize your data

=item L<PDL::Graphics::Prima::Scaling>

Specifies different kinds of scaling, including linear and logarithmic

=item L<PDL::Graphics::Prima::Simple>

Defines a number of useful functions for generating simple and not-so-simple
plots

=back

=head1 LICENSE AND COPYRIGHT

Portions of this module's code are copyright (c) 2011 The Board of Trustees at
the University of Illinois.

Portions of this module's code are copyright (c) 2011-2012 Northwestern
University.

This module's documentation are copyright (c) 2011-2012 David Mertens.

All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
