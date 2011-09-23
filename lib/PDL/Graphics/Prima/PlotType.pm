use strict;
use warnings;

# Note: The base class is defined near the bottom of the file because I wanted
# to write the documentation for creating new derived classes in the same space
# as where I wrote the code for the base class.

=head1 NAME

PDL::Graphics::Prima::PlotType - a collection of plot types

=head1 DESCRIPTION

This module provides all of the different plot types that you can use in a
PDL::Graphics::Prima widget. The documentation that follows is broken into three
parts:

=over

=item Line-based plot types

Many plots are based on plotting points of data or lines, or perhaps shaded
areas. If you think of your data as a function of a single variable, like a
time series, you will likely use these plot types to visualize your data.

=item Grid-based plot types

Other plots focus on using color or greyscale to visualize data that is a
function of two variables. If you need to plot a 2D histogram or you want to
visualize the elements of a matrix, you will likely use these plot types.

=item Creating new plot types

If the supplied plot types do not match your needs, you can always make a new
one: all of the code for all of these plot types is written in Perl, so it isn't
too difficult. This section describes how to create custom plot types for your
own needs.

=back

=cut

################################################################################
#                            Line-based Plot Types                            #
################################################################################

=head1 One-Dimensional Plot Types

=cut

#########################################
# PDL::Graphics::Prima::PlotType::Lines #
#########################################

=head2 Lines

Draws the x/y data as lines. This lets you draw each curve with an individual
color and line style; it does not let you specify a color and/or line style for
each segment.

=cut

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

# Collation needs some work, since the threading doesn't work quite right
# out-of-the-box. The line data is polyline data and it must be reduced
# first.
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::polylines_props);
	
	# Extract the line widths, against which we'll collate:
	my $lineWidths = $properties{lineWidths};
	$lineWidths = $self->widget->lineWidth unless defined $lineWidths;
	delete $properties{lineWidths};
	# get the rest of the piddles; we don't need their names:
	my @prop_piddles = values %properties;
	
	# Get the data:
	my ($xs, $ys) = $self->dataset->get_data;
	my ($min_x, $min_y, $max_x, $max_y) = PDL::minmaxforpair($xs, $ys);
	
	# working here - now that minmaxforpair does not return infs, make sure
	# this works
	my ($min_to_check, $max_to_check) = ($min_x, $max_x);
	($min_to_check, $max_to_check) = ($min_y, $max_y) if $axis_name eq 'y';
	
	# Collate the min and the max:
	return PDL::collate_min_max_wrt_many($min_to_check, $lineWidths,
			$max_to_check, $lineWidths, $pixel_extent, @prop_piddles);
}


# I need to define a drawing method:
sub draw {
	my ($self) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::polylines_props);

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->dataset->get_data_as_pixels;

	# Draw the lines:
	$self->widget->pdl_polylines($xs, $ys, %properties);
}

##########################################
# PDL::Graphics::Prima::PlotType::Spikes #
##########################################
# working here - get rid of the class-specific padding; if anything, such
# padding should be part of the general class, not this specific one

=head2 Spikes

Draws x/y data as a collection of vertical or horizontal lines. In the default
behavior, for each (x, y) data point, it draws a line from (x, 0) to (x, y). You
can change the baseline by specifying either the C<y_baseline> or C<x_baseline>
key. For example, by specifying C<< y_baseline => 5 >>, this will draw a lines
starting from (x, 5) instead of (x, 0). Specifying C<< x_baseline => -2 >> will
lead to horizontal lines instead of vertical lines, drawn from (-2, y) to
(x, y). Finally, if you specify the undefined value, as
C<< x_baseline => undef >> or C<< x_baseline => undef >>, the baseline will be
taken as the minimum of the dataset's x or y data, respectively.

=cut

package PDL::Graphics::Prima::PlotType::Spikes;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# Install the short name constructor:
sub pt::Spikes {
	PDL::Graphics::Prima::PlotType::Spikes->new(@_);
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
	my ($self) = @_;
	
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
		$widget->pdl_lines($xs, $ys, $xs, $baseline, %properties);
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
		$widget->pdl_lines($xs, $ys, $baseline, $ys, %properties);
	}
}



#########################################
# PDL::Graphics::Prima::PlotType::Blobs #
#########################################

=head2 Blobs

Lets you draw filled ellipses with per-point x- and y- pixel radii. If you
specify the key C<radius>, it draws filled circles with the given radius. The
more specific keys C<xRadius> and C<yRadius> override the C<radius> key.

=cut

package PDL::Graphics::Prima::PlotType::Blobs;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

use PDL::Core ':Internal';
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
		$x_radius = topdl($x_radius);
		$y_radius = topdl($y_radius);
		1;
	} or croak('Radii must be piddles, or values that can be interpreted by the pdl constructor');
	
	croak('Radii must be greater than or equal to 1')
		unless PDL::all($x_radius > 1) and PDL::all($y_radius > 1);
	
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
	my ($self) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::fill_ellipses_props);
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $self->dataset->get_data_as_pixels;

	# plot it:
	$self->widget->pdl_fill_ellipses($xs, $ys, 2*$self->{xRadius}, 2*$self->{yRadius}
		, %properties);
}

##########################################
# PDL::Graphics::Prima::PlotType::Points #
##########################################

package PDL::Graphics::Prima::PlotType::Points;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

#############################################
# PDL::Graphics::Prima::PlotType::Histogram #
#############################################

=head2 Histogram

working here - document

keys: topPadding, baseline

=cut

package PDL::Graphics::Prima::PlotType::Histogram;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

use Carp 'croak';
use PDL;
use strict;
use warnings;

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
	
	# Default to an upper padding of of 10 pixels:
	$self->{topPadding} = 10 unless defined $self->{topPadding};
	croak("topPadding must be a nonnegative integer")
		unless $self->{topPadding} =~ /^\d+$/ and $self->{topPadding} >= 0;
	
	# Make sure we have a default baseline:
	$self->{baseline} = 0 unless defined $self->{baseline};
	$self->{baseline} += 0;
}

use PDL::NiceSlice;

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
		my $padding = $self->{topPadding};
		return PDL::collate_min_max_wrt_many($ys, $lineWidths
			, $ys, $lineWidths + $padding, $pixel_extent
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
	my ($self) = @_;
	my $dataset = $self->dataset;
	my $widget = $self->widget;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::rectangles_props);
	
	# Get the edges and convert everything to pixels:
	my $edges = $self->get_bin_edges($dataset, $widget);
	my $pixel_edges = $widget->x->reals_to_pixels($edges);
	my $pixel_bottom = $widget->y->reals_to_pixels($self->{baseline});
	my $ys = $widget->y->reals_to_pixels($dataset->get_ys);
	
	$widget->pdl_rectangles($pixel_edges(0:-2), $pixel_bottom
			, $pixel_edges(1:-1), $ys, %properties);
}

#################################################
# PDL::Graphics::Prima::PlotType::BoxAndWhisker #
#################################################
# Plots vertical box-and-whisker at each data point

package PDL::Graphics::Prima::PlotType::BoxAndWhisker;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

#############################################
# PDL::Graphics::Prima::PlotType::ErrorBars #
#############################################
# Adds error bars

package PDL::Graphics::Prima::PlotType::ErrorBars;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# working here - ensure documentation consistency

=head2 ErrorBars

You create an error bars plotType objet with C<pt::ErrorBars>:

 pt::ErrorBars(x_err => 10);

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

 err_width   - width of both error bars
 x_err_width - width of x-error bars
 y_err_width - width of y-error bars

Again, the more specific widths override the less specific ones.

=cut

# Install the short name constructor:
sub pt::ErrorBars {
	PDL::Graphics::Prima::PlotType::ErrorBars->new(@_);
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
	$self->{upper_bars} = $bars->abs if defined $bars;
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
	my ($self) = @_;
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
		my $local_ys = $widget->y->reals_to_pixels($ys); #--

		# Draw the line from the point to the edge:
		$widget->pdl_lines($left_xs, $local_ys, $local_xs, $local_ys, %properties);
		# Draw the end caps:
		my $width = $self->{x_err_width}/2;
		$widget->pdl_lines($left_xs, $local_ys + $width, $left_xs
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
		$widget->pdl_lines($local_xs, $local_ys, $right_xs, $local_ys, %properties);
		# Draw the end caps:
		my $width = $self->{x_err_width} / 2;
		$widget->pdl_lines($right_xs, $local_ys + $width, $right_xs
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
		$widget->pdl_lines($local_xs, $local_ys, $local_xs, $upper_ys, %properties);
		# Draw the caps:
		my $width = $self->{y_err_width} / 2;
		$widget->pdl_lines($local_xs - $width, $upper_ys, $local_xs + $width
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
		$widget->pdl_lines($local_xs, $local_ys, $local_xs, $lower_ys, %properties);
		# Draw the caps
		my $width = $self->{y_err_width} / 2;
		$widget->pdl_lines($local_xs - $width, $lower_ys, $local_xs + $width
			, $lower_ys, %properties);
	}
}

#########################################
# PDL::Graphics::Prima::PlotType::Bands #
#########################################
# Plots symmetric or unsymmetric error bands around the data

package PDL::Graphics::Prima::PlotType::Bands;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

########################################
# PDL::Graphics::Prima::PlotType::Area #
########################################
# Plots shaded area, where one edge is the data

package PDL::Graphics::Prima::PlotType::Area;
our @ISA = qw(PDL::Graphics::Prima::PlotType);


###############################################################################
#                            Grid-based Plot Types                            #
###############################################################################

=head1 Plotting Two-Dimensional Data

=cut

#############################################
# PDL::Graphics::Prima::PlotType::GridLines #
#############################################

package PDL::Graphics::Prima::PlotType::GridLines;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

# working here

#############################################
# PDL::Graphics::Prima::PlotType::ColorGrid #
#############################################
# Plots a matrix

package PDL::Graphics::Prima::PlotType::ColorGrid;
our @ISA = qw(PDL::Graphics::Prima::PlotType);

use Carp 'croak';

=head2 ColorGrid

This plot type lets you specify colors or values on a grid, primarily for making
color contour plots (as opposed to line contour plots). Put differently, it lets
you visualize a two-dimensional histogram by using colors. It also forms the
basis for the Matrix and Func2D plot types. This plot type uses the colors, x,
and y piddles a bit differently than the other plot types---in particular, it
requires that you supply a value for colors. However, you can safely mix it with
other plot types if you wish.

ColorGrid (and its derivatives) uses the x and y data to determine the grid
dimensions. In the simplest use case, the x and y piddles each contain two
elements, representing the grid's x min/max and y min/max. In that case, the
spacing between each of the grid's rectangles is sampled lineary. Alternatively,
you can specify all the x and y grid boundaries, in which case if the colors
matrix has dimensions M x N, you will need to specify M + 1 values for x and
N + 1 values for y.

At this point, you should see the potential pitfalls of mixing this plot type
with others. If you plot a quadratic function and include a ColorGrid plot type,
the ColorGrid will interpret the x and y values as being 

=cut

# Install the short-name constructor:
sub pt::ColorGrid {
	PDL::Graphics::Prima::PlotType::ColorGrid->new(@_);
}

# Needed for function topdl:
use PDL::Core ':Internal';

# In the initialization, check that they supplied a color grid. I'd check the
# x and y data, but I don't have access to that here:
sub initialize {
	my $self = shift;
	$self->SUPER::initialize(@_);
	
	# Check that they have colors and that it is a meaningful value:
	croak("You must supply colors to ColorGrid")
		unless exists $self->{colors};
	eval { $self->{colors} = topdl $self->{colors} };
	if ($@) {
		my $to_croak = $@;
		$to_croak =~ s/ at.*/.\n/s;
		$to_croak .= "  You must supply a piddle or an array ref for ColorGrid's colors";
		croak($to_croak);
	}
	
	# If they supplied xs and/or ys, set them up as piddles:
	if (exists $self->{xs}) {
		# Ensure we're working with a piddle and not something else:
		eval { $self->{xs} = topdl($self->{xs}) };
		# If tha last line ran into trouble, it's because they didn't supply
		# a meaningful expression for the xs, so croak:
		if ($@) {
			my $to_croak = $@;
			$to_croak =~ s/ at.*/.\n/s;
			$to_croak .= "  You must supply a piddle or an array ref for ColorGrid's xs";
			croak($to_croak);
		}
		
		# Now make sure that we have the correct dimensions:
		my $xs = $self->{xs};
		
		croak("xs first dimension must either be of length 2 or M+1, where"
			. " M is the size of color's first dim")
				unless $xs->dim(0) == 2
					or $xs->dim(0) == $self->{colors}->dim(0) + 1;
	}
	if (exists $self->{ys}) {
		# Ensure we're working with a piddle and not something else:
		eval { $self->{ys} = topdl($self->{ys}) };
		# If tha last line ran into trouble, it's because they didn't supply
		# a meaningful expression for the ys, so croak:
		if ($@) {
			my $to_croak = $@;
			$to_croak =~ s/ at.*/.\n/s;
			$to_croak .= "  You must supply a piddle or an array ref for ColorGrid's ys";
			croak($to_croak);
		}
		
		# Now make sure that we have the correct dimensions:
		my $ys = $self->{ys};
		
		croak("ys first dimension must either be of length 2 or N+1, where"
			. " N is the size of color's second dim")
				unless $ys->dim(0) == 2
					or $ys->dim(0) == $self->{colors}->dim(1) + 1;
	}
}

# Collation function is really, really simple
sub compute_collated_min_max_for {
	my ($self, $axis_name, $pixel_extent) = @_;
	# Get the list of properties for which we need to look for bad values:
	my %properties
		= $self->generate_properties(@PDL::Drawing::Prima::bars_props);
	
	my ($xs, $ys) = $self->dataset->get_data;
	my $widget = $self->widget;
	# Use the custom xs and ys if supplied:
	$xs = $self->{xs} if exists $self->{xs};
	$ys = $self->{ys} if exists $self->{ys};
	
	my ($to_check, $extra) = ($xs, $ys);
	($to_check, $extra) = ($ys, $xs) if $axis_name eq 'y';
	
	# Fudging a little bith with $extra, this could be improved
	# working here
	return PDL::collate_min_max_wrt_many($to_check(:-2), 0, $to_check(1:), 0
		, values %properties, $extra(1:));
}

# Don't need to supply x or y min/max functions

sub draw {
	my ($self) = @_;
	my ($xs, $ys) = $self->dataset->get_data;
	my $widget = $self->widget;
	# Use the custom xs and ys if supplied:
	$xs = $self->{xs} if exists $self->{xs};
	$ys = $self->{ys} if exists $self->{ys};
	
	# Pull the colors out of self:
	my $colors = $self->{colors};
	
	# sort the xs and ys:
	$xs = $xs->qsort;
	$ys = $ys->qsort;
	
	# How we proceed depends on how many dimensions x and y have:
	if ($xs->dim(0) == 2) {
		my @dims = $xs->dims;
		$dims[0] = $colors->dim(0) + 1;
		# Create N+1 values that run exactly from 0 to 1:
		my $new_xs = PDL->zeroes(@dims)->xvals / $colors->dim(0);
		# Rescale the xs by the widths x(1) - x(0):
		$new_xs *= $xs(1) - $xs(0);
		# Shift the xs by the min, x(0):
		$new_xs += $xs(0);
		# Finally, set the variable xs to hold these new values. This will not
		# overwrite the original data from the dataset:
		$xs = $new_xs;
	}
	elsif ($xs->dim(0) != $colors->dim(0) + 1) {
		# Check that the the x- and color-dims agree:
		croak("ColorGrid: x-data's first dimension must be of size M+1, "
			. "where M is the size of color's first dimension");
	}
	if ($ys->dim(0) == 2) {
		my @dims = $ys->dims;
		$dims[0] = $colors->dim(1) + 1;
		# Create N+1 values that run exactly from 0 to 1:
		my $new_ys = PDL->zeroes(@dims)->xvals / $colors->dim(1);
		# Rescale the ys by the widths y(1) - y(0):
		$new_ys *= $ys(1) - $ys(0);
		# Shift the ys by the min, y(0):
		$new_ys += $ys(0);
		# Finally, set the variable ys to hold these new values. This will not
		# overwrite the original data from the dataset:
		$ys = $new_ys;
	}
	elsif ($ys->dim(0) != $colors->dim(1) + 1) {
		# Check that the the y- and color-dims agree:
		croak("ColorGrid: y-data's first dimension must be of size N+1, "
			. "where N is the size of color's second dimension");
	}
	
	# Convert the xs and ys to the coordinate values:
	$ys = $widget->y->reals_to_pixels($ys);
	$xs = $widget->x->reals_to_pixels($xs);
	
	# Gather the properties that I will need to use in the plotting. Be sure to
	# keep the props (generated above) in sync with the actual Prima drawing
	# command used below.
	my %properties = $self->generate_properties(@PDL::Drawing::Prima::bars_props);
	
	# Set up the x- and y- dimension lists for proper threading:
	$xs = $xs->dummy(1, $colors->dim(1));
	$ys = $ys->dummy(0, $colors->dim(0));

	# Now draw the bars for each rectangle:
	$widget->pdl_bars($xs(:-2), $ys(,:-2), $xs(1:), $ys(,1:), %properties);
}

##########################################
# PDL::Graphics::Prima::PlotType::Matrix #
##########################################
# Plots a matrix

package PDL::Graphics::Prima::PlotType::Matrix;
our @ISA = qw(PDL::Graphics::Prima::PlotType::ColorGrid);

=head2 Matrix

Makes a simple visualization of a matrix. 

=cut

###############################################################################
#                         Creating your own Plot Type                         #
###############################################################################
# Also includes the base type

package PDL::Graphics::Prima::PlotType;

use Carp 'croak';

=head1 Creating your own Plot Type

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

=head2 xmin, xmax, ymin, ymax

Note: these are likely to change in the future.

These plotType functions are called when the graph needs to determine automatic
minima and maxima. Line plots simply return the data's minimum and maximum, but
more complex plot types, such as those including error bars or blobs, need to
take more details into consideration.

These functions are always called with three arguments and should always return
two values. The three arguments are:

=over

=item plotType

This is whatever you created with your constructor; if you're following the
example above, that would be an instance of the plotType. This passes in
whatever the dataset was given for the plotType.

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
	return collate_min_max_wrt_many($to_check, $lineWidths
		, $to_check, $lineWidths, $pixel_extent, $extra, @prop_piddles);
}

=head2 generate_properties

Needs to be explained. Basically, this accumulates all the properties from the
plotType object together with thsoe from the dataset into a single hash that
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
	
	return %properties;
}

=head2 widget

working here

=head2 dataset

working here

=cut

sub widget {
	$_[0]->{widget} = $_[1] if (@_ == 2);
	return $_[0]->{widget};
}

sub dataset {
	$_[0]->{dataSet} = $_[1] if (@_ == 2);
	return $_[0]->{dataSet};
}

=head2 draw

Needs explanation and examples. This function will be called whenever the
plot widget needs to redraw your plotType (window resizes, zooms, etc). It is
called with three arguments: the plotType object, the dataSet object, and
the widget object.

Now, something that I I<always> forget to do is to convert the data values to
pixel values. You do that with the widget's x- and y-axis objects with code like

 my $x_to_plot = $widget->x->reals_to_pixels($xs)

If it seems like your new plot type is not plotting anything, be sure that you
have properly converted the points you are trying to plot.

=cut

sub draw {
	my $invocant = shift;
	my $class = ref($invocant) ? ref($invocant) : $invocant;
	croak("Plot type $class does not define its own drawing function. Please "
		. 'report this bug to the author');
}


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
 	base_class => 'PDL::Graphics::Prima::PlotType::Blobs',
 	draw => sub {
 		my ($self, $dataset, $widget) = @_;
 		
 		# Retrieve the data from the dataset:
 		my ($xs, $ys) = $dataset->get_data_as_pixels($widget);
 		
 		# Draw the smileys:
 		$widget->pdl_ellipses($xs, $ys, 10, 10);	# face
 		$widget->pdl_fill_ellipses($xs - 5, $ys + 4, 2, 2);	# left eye
 		$widget->pdl_fill_ellipses($xs + 5, $ys + 4, 2, 2); # right eye
 		$widget->pdl_fill_chords($xs, $ys + 3, 10, 10, 200, 340); # smiling mouth
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
	$self->{base_class} ||= 'PDL::Graphics::Prima::PlotType';
	
	# Call the superclass initialization:
	my $init = $self->{base_class}->can('initialize');
	&$init($self, @_);
	
	# Make sure that we have a valid draw function:
	if ($self->{base_class} eq 'PDL::Graphics::Prima::PlotType') {
		croak('You must supply a draw function or a drawable base class to the CallBack plot class')
			unless exists $self->{draw} and ref ($self->{draw})
				and ref ($self->{draw}) eq 'CODE';
		
	}
}

# Dynamic drawing based upon values of the draw key and/or the base class:
sub draw {
	my ($self, $dataset, $widget) = @_;
	
	if (not exists $self->{draw}) {
		# Didn't supply a draw function, so call the base class's draw function:
		if ($self->{base_class} eq 'PDL::Graphics::Prima::PlotType') {
			croak('CallBack plot type must supply a drawing function or '
				. 'a base class that can draw itself.');
		}
		
		# Masquerade as the base class:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->draw($dataset, $widget);
		bless $self, $class;
		return;
	}
	
	if (ref($self->{draw}) and ref($self->{draw}) eq 'CODE') {
		# They supplied a code reference, so run it:
		my $func = $self->{draw};
		&$func($self, $dataset, $widget);
		return;
	}
	
	croak('You must supply a code reference for your drawing code.');
}




=head1 AUTHOR, COPYRIGHT

This module was written by David Mertens.

Copyright 2011, David Mertens, all rights reserved. This library is free
software; you can redistribute it and/or modify it under the same tersm as Perl
itself.

=cut

1;
