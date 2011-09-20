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

# The min/max functions work just fine for lines, but I need to define a drawing
# method:
sub draw {
	my ($self, $dataset, $widget) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties($dataset
		, @PDL::Drawing::Prima::polylines_props);

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);

	# Draw the lines:
	$widget->pdl_polylines($xs, $ys, %properties);
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
}

# Override the min/max functions to give padding values:
sub xmin {
	my ($self, $dataset, $widget) = @_;
	
	# Call the superclass xmin function, ignoring the returned padding:
	my ($xmin) = $self->SUPER::xmin($dataset, $widget);
	
	# If the baseline is an x-baseline (horizontal lines), we will consider an
	# x-min of whatever they indicated; otherwise, the data's x-min is the x-min
	# of interest:
	if (exists $self->{x_baseline} and not defined $self->{x_baseline}) {
		return ($xmin, 1);
	}
	elsif (exists $self->{x_baseline}) {
		return ($self->{x_baseline}, 1) if $self->{x_baseline} < $xmin;
		return ($xmin, 1);
	}
	
	# Otherwise, the baseline is a y-baseline, in which case we are drawing
	# vertical lines. The xmin is straight-forward but we must take the
	# linewidths into account for the padding.
	if (exists $self->{lineWidths}) {
		my $width = $self->{lineWidths}->max;
		return ($xmin, $width);
	}
	# Otherwise, return a padding of 1:
	return ($xmin, 1);
}

sub xmax {
	my ($self, $dataset, $widget) = @_;
	
	# Call the superclass xmax function, ignoring the returned padding:
	my ($xmax) = $self->SUPER::xmax($dataset, $widget);
	
	# If the baseline is an x-baseline (horizontal lines), we will consider an
	# x-max of whatever they indicated in case all the x-data is less than the
	# baseline. In the special case of the undefined baseline, the max will
	# always be the data's max since the baseline is the data's min:
	if (exists $self->{x_baseline} and not defined $self->{x_baseline}) {
		return ($xmax, 1);
	}
	elsif (exists $self->{x_baseline}) {
		return ($self->{x_baseline}, 1) if $self->{x_baseline} > $xmax;
		return ($xmax, 1);
	}

	# Otherwise, the baseline is a y-baseline, in which case we are drawing
	# vertical lines. The xmin is straight-forward but we must take the
	# linewidths into account for the padding.
	if (exists $self->{lineWidths}) {
		my $width = $self->{lineWidths}->max;
		return ($xmax, $width);
	}
	# Otherwise, return a padding of 1:
	return ($xmax, 1);
}

sub ymin {
	my ($self, $dataset, $widget) = @_;
	
	# Call the superclass ymin function, ignoring the returned padding:
	my ($ymin) = $self->SUPER::ymin($dataset, $widget);
	
	# If the baseline is an y-baseline (vertical lines), we will consider an
	# y-min of whatever they indicated; otherwise, the data's y-min is the y-min
	# of interest:
	if (exists $self->{y_baseline} and not defined $self->{y_baseline}) {
		return ($ymin, 1);
	}
	elsif (exists $self->{y_baseline}) {
		return ($self->{y_baseline}, 1) if $self->{y_baseline} < $ymin;
		return ($ymin, 1);
	}
	
	# Otherwise, the baseline is an x-baseline, in which case we are drawing
	# horizontal lines. The ymin is straight-forward but we must take the
	# linewidths into account for the padding.
	if (exists $self->{lineWidths}) {
		my $width = $self->{lineWidths}->max;
		return ($ymin, $width);
	}
	# Otherwise, return a padding of 1:
	return ($ymin, 1);
}

sub ymax {
	my ($self, $dataset, $widget) = @_;
	
	# Call the superclass ymax function, ignoring the returned padding:
	my ($ymax) = $self->SUPER::ymax($dataset, $widget);
	
	# If the baseline is an y-baseline (vertical lines), we will consider an
	# y-max of whatever they indicated in case all the y-data is less than the
	# baseline. In the special case of the undefined baseline, the max will
	# always be the data's max since the baseline is the data's min:
	if (exists $self->{y_baseline} and not defined $self->{y_baseline}) {
		return ($ymax, 1);
	}
	elsif (exists $self->{y_baseline}) {
		return ($self->{y_baseline}, 1) if $self->{y_baseline} > $ymax;
		return ($ymax, 1);
	}

	# Otherwise, the baseline is an x-baseline, in which case we are drawing
	# horizontal lines. The ymax is straight-forward but we must take the
	# linewidths into account for the padding.
	if (exists $self->{lineWidths}) {
		my $width = $self->{lineWidths}->max;
		return ($ymax, $width);
	}
	# Otherwise, return a padding of 1:
	return ($ymax, 1);
}

# Here is the method for drawing spikes:
sub draw {
	my ($self, $dataset, $widget) = @_;
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties($dataset
		, @PDL::Drawing::Prima::lines_props);

	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);
	
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
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties($dataset
		, @PDL::Drawing::Prima::fill_ellipses_props);
	
	# Retrieve the data from the dataset:
	my ($xs, $ys) = $dataset->get_data_as_pixels($widget);

	# plot it:
	$widget->pdl_fill_ellipses($xs, $ys, 2*$self->{xRadius}, 2*$self->{yRadius}
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

key: topPadding

=cut

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
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties($dataset
		, @PDL::Drawing::Prima::rectangles_props);
	
	# Get the edges and convert everything to pixels:
	my $edges = $self->get_bin_edges($dataset, $widget);
	my $pixel_edges = $widget->x->reals_to_pixels($edges);
	my $pixel_bottom = $widget->y->reals_to_pixels(0);
	my $ys = $widget->y->reals_to_pixels($dataset->get_ys($widget));
	
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

# The various min/max functions
sub xmin {
	my ($self, $dataset, $widget) = @_;

	# Return the minimum of the data modified by the error bars, as they exist.
	my ($xs, $ys) = $dataset->get_data($widget);
	$xs = $xs - $self->{left_bars} if exists $self->{left_bars};
	my ($xmins) = PDL::minmaxforpair($xs, $ys);
	
	# Add padding for y error-bars if present:
	return ($xmins->min, $self->{y_err_width})
		if $self->y_bars_present;
	# Otherwise we don't need any extra space:
	return ($xmins->min, 1);
}

sub xmax {
	my ($self, $dataset, $widget) = @_;

	# Return the maximum of the data modified by the error bars, as they exist.
	my ($xs, $ys) = $dataset->get_data($widget);
	$xs = $xs + $self->{right_bars} if exists $self->{right_bars};
	my (undef, undef, $xmaxes) = PDL::minmaxforpair($xs, $ys);
	
	# Add padding for y error-bars if present:
	return ($xmaxes->max, $self->{y_err_width})
		if $self->y_bars_present;
	# Otherwise we don't need any extra space:
	return ($xmaxes->max, 1);
}

sub ymin {
	my ($self, $dataset, $widget) = @_;

	# Return the minimum of the data modified by the error bars, as they exist.
	my ($xs, $ys) = $dataset->get_data($widget);
	$ys = $ys - $self->{lower_bars} if exists $self->{lower_bars};
	my (undef, $ymins) = PDL::minmaxforpair($xs, $ys);
	
	# Add padding for x error-bars if present:
	return ($ymins->min, $self->{x_err_width})
		if $self->x_bars_present;
	# Otherwise we don't need any extra space:
	return ($ymins->min, 1);
}

sub ymax {
	my ($self, $dataset, $widget) = @_;

	# Return the maximum of the data modified by the error bars, as they exist.
	my ($xs, $ys) = $dataset->get_data($widget);
	$ys = $ys + $self->{upper_bars} if exists $self->{upper_bars};
	my (undef, undef, undef, $ymaxes) = PDL::minmaxforpair($xs, $ys);
	
	# Add padding for x error-bars if present:
	return ($ymaxes->max, $self->{x_err_width})
		if $self->x_bars_present;
	# Otherwise we don't need any extra space:
	return ($ymaxes->max, 1);
}

sub draw {
	my ($self, $dataset, $widget) = @_;
	my ($xs, $ys) = $dataset->get_data($widget);
	
	# Assemble the various properties from the plot-type object and the dataset
	my %properties = $self->generate_properties($dataset
		, @PDL::Drawing::Prima::lines_props);
	
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

sub xmin {
	my ($self, $dataset, $widget) = @_;

	# Ostensibly, this version of xmin doesn't care what invocant you called it
	# with because it does not need any information from self. I unpack all of
	# the arguments here to illustrate the calling order.
	
	# Return the minimum of the data with a padding of 1 pixel. Note that this
	# assumes that the data at $dataset->[0] is a piddle if it is not a code
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
	my ($self, $dataset, @prop_list) = @_;
	my %properties;
	
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

# working here - this isn't working the way it's supposed to

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

# Dynamic xmax processing based upon current value of xmax key:
sub xmax {
	my ($self, $dataset, $widget) = @_;
	
	# Return an xmax that depends on what they supplied:
	if (not exists $self->{xmax}) {
		# No xmax specified. Use the base class's xmax:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->xmax($dataset, $widget);
		bless $self, $class;
	}
	elsif (not ref($self->{xmax})) {
		# No ref means they gave a scalar for xmax, which means they want to
		# use the default padding of 1:
		return ($self->{xmax}, 1);
	}
	elsif (ref($self->{xmax}) eq 'ARRAY') {
		# Array ref for xmax means they gave both value and padding:
		return @{$self->{xmax}};
	}
	elsif (ref($self->{xmax}) eq 'CODE') {
		# Code ref means they supplied their own code:
		my $func = $self->{xmax};
		return &$func($self, $dataset, $widget);
	}
}

# Dynamic xmin processing based upon current value of xmin key:
sub xmin {
	my ($self, $dataset, $widget) = @_;
	
	# Return an xmin that depends on what they supplied:
	if (not exists $self->{xmin}) {
		# No xmin specified. Use the base class's xmin:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->xmin($dataset, $widget);
		bless $self, $class;
	}
	elsif (not ref($self->{xmin})) {
		# No ref means they gave a scalar for xmin, which means they want to
		# use the default padding of 1:
		return ($self->{xmin}, 1);
	}
	elsif (ref($self->{xmin}) eq 'ARRAY') {
		# Array ref for xmin means they gave both the value and the padding:
		return @{$self->{xmin}};
	}
	elsif (ref($self->{xmin}) eq 'CODE') {
		# Code ref means they supplied their own code:
		my $func = $self->{xmin};
		return &$func($self, $dataset, $widget);
	}
}

# Dynamic ymax processing based upon current value of ymax key:
sub ymax {
	my ($self, $dataset, $widget) = @_;
	
	# Return an ymax that depends on what they supplied:
	if (not exists $self->{ymax}) {
		# No ymax specified. Use the base class's ymax:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->ymax($dataset, $widget);
		bless $self, $class;
	}
	elsif (not ref($self->{ymax})) {
		# No ref means they gave a scalar for ymax, which means they want to
		# use the default padding of 1:
		return ($self->{ymax}, 1);
	}
	elsif (ref($self->{ymax}) eq 'ARRAY') {
		# Array ref for ymax means they gave both value and padding:
		return @{$self->{ymax}};
	}
	elsif (ref($self->{ymax}) eq 'CODE') {
		# Code ref means they supplied their own code:
		my $func = $self->{ymax};
		return &$func($self, $dataset, $widget);
	}
}

# Dynamic ymin processing based upon current value of ymin key:
sub ymin {
	my ($self, $dataset, $widget) = @_;
	
	# Return an ymin that depends on what they supplied:
	if (not exists $self->{ymin}) {
		# No ymin specified. Use the base class's ymin:
		my $class = ref($self);
		bless $self, $self->{base_class};
		$self->ymin($dataset, $widget);
		bless $self, $class;
	}
	elsif (not ref($self->{ymin})) {
		# No ref means they gave a scalar for ymin, which means they want to
		# use the default padding of 1:
		return ($self->{ymin}, 1);
	}
	elsif (ref($self->{ymin}) eq 'ARRAY') {
		# Array ref for ymin means they gave both the value and the padding:
		return @{$self->{ymin}};
	}
	elsif (ref($self->{ymin}) eq 'CODE') {
		# Code ref means they supplied their own code:
		my $func = $self->{ymin};
		return &$func($self, $dataset, $widget);
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
