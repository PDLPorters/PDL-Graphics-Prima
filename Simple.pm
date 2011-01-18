use strict;
use warnings;

package Prima::Graph::Simple;
use base 'Prima::Widget';

use Carp 'croak';
use Prima;
use PDL;
use PDL::NiceSlice;

=head1 NAME

Prima::Graph::Simple - a simple graphing extension for Prima

=head1 SYNOPSIS

 working here

=head1 STEPS

These are the things I need to do to have a bonafide widget:

=over

=item decide on a set of properties

For now, I'm going to keep everything in one big widget. This means all of the
axis settings, for example, will be properties of this widget. What should those
include?

logarithmic or linear scaling, axis limits, tick mark details, axis label text
and positioning, data.

I have an idea for the data that I think will make it easy. Adding a data set
involves passing an anonymous array with the arguments that you would have
supplied to the polylines routine. The drawing of this data will then simply
involve rescaling the values to the axis limits and calling the polylines
function.

=item write accessors 

=item create a default profile

=item write initialization and destruction code (if there is anything special)

=item set up any notifications

=back

=head1 PROPERTIES

These are the items that you can easily get or set.

=head2 xleftBoundary, xrightBoundary, ybottomBoundary, ytopBoundary,
xboundaries, yboundaries, boundaries

The boundaries of the plotting region. The can be either percentages or pixel
counts. Both must be passed in as strings, since raw numbers refer to coordinate
values for other properties, but coordinate values are not defined for setting
boundaries.

For example:

 $graph->xleftBoundary('10%');
 # the next two are equivalent:
 $graph->yboundaries('10%', '90%');
 $graph->yboundaries('10%');

For setters that set more than one boundary, specifying on a single string
will result in that string's complement being used for the opposit boundary.

This could probably make use of the various geometry options available in Prima
to set flexible minima and maxima, but those will have to wait.

=cut

sub good_boundary {
	my $boundary = shift;
	
	if ($boundary !~ /^\d+(%|p)$/) {
		# bad, set $@
		return 0;
	}
	
	return 1;
}

sub xleftBoundary {
	my ($self, $xlb) = @_;
	return $self->{x_left_boundary} unless defined $xlb;
	if (not good_boundary($xlb)) {
		croak($@);
	}
}

#sub viewport {
#	my $self = shift;
#	return @{$self->{viewport}} unless $#_;
#	croak("Viewport must be a four-element array reference")
#		unless ref($new_viewport) and ref($new_viewport) eq 'ARRAY'
#			and @$new_viewport == 4;
#	foreach(@$new_viewport) {
#		croak("Viewport values must be between "
#	}
#	$self->{viewport} = $new_viewport;
#}

=head2 TickLength, tickLength, xTickLength, yTickLength

Major and minor tick lengths. These are specified with strings or with anonymous
arrays with pairs of strings. The strings can have one of three formats:

 Length as a percentage of the viewport: "2%"
 Length in pixels: "5p"
 Length in coordinates: 1.2

Lengths can be positive or negative. Positive lengths are drawn within the
viewport whereas negative lengths are drown outside the viewport. If you specify
a string pair using an anonymous array, you indicate where the tick starts and
where the tick stops, so that you can have ticks both below and above the axis,
or floating away from the axis, if you like.

Examples:

 # Sets ticks both inside and outside the viewport
 $graph->TickLength(['-2%', '2%')]);

=head2 xTickStep, yTickStep 

Controls the step size between major tick marks. As with tick length, these can
be specified in one of three ways: a percentage, a number of pixels, or a
coordinate distance. However, these must be strictly positive.

=head2 xtickCount, ytickCount

Sets the number of minor ticks between major ticks.

=head2 xmin, xmax, ymin, ymax, xminmax, yminmax, minima, maxima, limits

These set the bounds of the plot. The can take real values, the string 'fixed',
or strings with a percent at the end. For example, to view all your data,
automatically adjusting even as you added new data, you would use this:

 $graph->limits('0%', '0%', '100%', '100%');

If you like the x-maximum and would like to keep the graph at the current value,
you would use:

 $graph->xmax('fixed');

Adding new data will not change the xmax.

=head2 xlabel, ylabel

Text strings indicating the labels.

=head2 xlabelPosition, ylabelPosition

Positioning settings. I should read more about the text positioning commands
before I push to far along these.

=cut

#####################
# Working live code #
#####################

=head1 GRAPHING

This module installs the C<graph> method into the Prima::Drawable
namespace, enabling all widgets to draw graphs. The arguments to
C<graph> are (1) anonymous arrays with the data you want to plot
together with their polylines arguments and (2) an anonymous hash with
the plot options. Plot options include:

=over

=item scaling

One of 'linear', 'logx', 'logy', or 'logxy'. This sets the scaling of
your plots.

=item xmin, xmax, ymin, ymax

The x- and y- clipping of your plot.

=item xlabel, ylabel

The x- and y- label for your plot.

=item viewport

The relative size of the plotting box of your graph. Four-element
anonymous array with values between 0 and 1, corresponding to left,
bottom, right, top. Default values are 0.1, 0.1, 0.9, and 0.9.

=back

=cut

sub rescale_to_pixels {
	my ($data, $data_min, $data_max, $pixel_extent, $pixel_offset) = @_;
	return (($data - $data_min) / ($data_max - $data_min)
			* $pixel_extent + $pixel_offset);
}

our $offset_penalty = 2;

sub _adjusted_position {
	my ($value, $interval, $offset, $direction) = @_;
	# Find the first copy of $interval that's less than $value:
	my $to_return = int($value / $interval) * $interval;
	# include the offset:
	$to_return += $offset;
	while(1) {
		return $to_return if $value == $to_return;
		return $to_return if (($to_return <=> $value) == $direction);
		$to_return += $direction * $interval;
	}
	# Return the final, adjusted position:
	return $to_return;
}

sub compute_linear_ticks {
	my ($min, $max) = @_;
	
	# This algorithm was designed with the following major tick layout
	# examples in mind:
	#	min  	max		ticks at
	#	2		4		2, 2.5, 3, 3.5, 4
	#	2.2		4.3		2.25, 2.75, 3.25, 3.75, 4.25 (maybe)
	#					2.4, 2.8, 3.2, 3.6, 4.0 (maybe)
	#	0		20		0, 5, 10, 15, 20
	#	30.5	37.5	31, 33, 35, 37
	#	0		112		0, 25, 50, 75, 100
	
	# Determine the order of magnitude of the min/max range:
	my $full_range = $max - $min;
	# Start by assuming that the order of magnitude will be a good
	# step size:
	my $exponent = log($max - $min) / log(10);
	# Correct for tracation of negative numbers:
	$exponent-- if $exponent < 0;
	my $order_of_magnitude = 10**(int $exponent);
	# Score each potential interval and take the lowest one:
	my $best_score = 100;
	my $best_Ticks;
	my $best_interval;
	my $best_scaled_interval;
	my $best_offset;
	
	my %intervals # interval	# handicap
		= qw(		0.25		0
					0.3			0.5
					0.4			0.25
					0.5			0
					0.6			0.5
					0.8			0.25
					1			0
					2			0.25
					2.5			0
					3			0.5
					4			0.25
		);
	
	while (my ($scaled_interval, $handicap) = each %intervals) {
		my $interval = $order_of_magnitude * $scaled_interval;
		for my $offset (0, $interval/2) {
			# The ticks will start at either a multiple of the interval,
			# or a multiple of the interval plus the offset. So,
			# recompute the range in light of the tick size:
			my $min_Tick = _adjusted_position($min, $interval, $offset, 1);
			my $max_Tick = _adjusted_position($max, $interval, $offset, -1);
			
			# Count the number of ticks between min_Tick and max_Tick
			my $N_Ticks = ($max_Tick - $min_Tick) / $interval + 1;
			
			# Obviously, max tick and min tick must not overlap:
			next if $N_Ticks < 2;
			
			# compute the score:
			$handicap += $offset_penalty if $offset != 0;
			my $score = (abs($N_Ticks - 5) + $handicap)**2;
			
			# If it's the best score, use it:
			if ($score < $best_score) {
				$best_score = $score;
				# Add 0.1 to N_Ticks to avoid rounding dilemas:
				$best_Ticks = zeroes($N_Ticks + 0.1)->xlinvals($min_Tick, $max_Tick);
				$best_interval = $interval;
				$best_scaled_interval = $scaled_interval;
				$best_offset = $offset;
			}
		}
	}
	
	# Construct the minor tick marks:
	my $tick_interval;
	if ($best_scaled_interval eq '0.3'
			or $best_scaled_interval eq '3'
			or $best_scaled_interval eq '0.6'
	) {
		$tick_interval = $best_interval / 6;
	}
	elsif (($best_scaled_interval eq '0.25'
			or $best_scaled_interval eq '0.5'
			or $best_scaled_interval eq '2.5'
			) and $best_offset == 0
	) {
		$tick_interval = $best_interval / 5;
	}
	else {
		$tick_interval = $best_interval / 4;
	}
	my $min_tick = _adjusted_position($min, $tick_interval, $best_offset, 1);
	my $max_tick = _adjusted_position($max, $tick_interval, $best_offset, -1);
	my $N_ticks = ($max_tick - $min_tick) / $tick_interval + 1.1;
	# Add 0.1 to escape rounding trouble.
	my $ticks = zeroes($N_ticks)->xlinvals($min_tick, $max_tick);

	# finally, set up zero values that make sense:
	$best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min)) .= 0;
	$ticks->where(abs($ticks) * 1e10 < ($max - $min)) .= 0;
	return ($best_Ticks, $ticks);
}

# This algorithm was based almost entirely off the linear ticks
# algorithm, with a few special tweaks.
sub compute_log_ticks {
	# These are the LOGARITHMS of the actual data's min and max:
	my ($min, $max) = @_;
	
	die "Not yet implemented";

=pod	

	# Determine the order of magnitude of the min/max range:
	my $full_range = $max - $min;
	# Start by assuming that the order of magnitude will be a good
	# step size:
	my $order_of_magnitude = 10**(int (log($max - $min) / log(10)));
	
	# Score each potential interval and take the lowest one:
	my $best_score = 100;
	my $best_Ticks;
	my $best_interval;
	my $best_scaled_interval;
	
	my %intervals # interval	# handicap
		= qw(		0.25		0
					0.3			0
					0.5			0
					1			0
					2			0.25
					2.5			0
					3			0.5
					4			0.25
		);
	
	while (my ($scaled_interval, $handicap) = each %intervals) {
		my $interval = $order_of_magnitude * $scaled_interval;
		# The ticks will start at either a multiple of the interval,
		# or a multiple of the interval plus the offset. So,
		# recompute the range in light of the tick size:
		my $min_Tick = _adjusted_log_position($min, $interval, 1);
		my $max_Tick = _adjusted_log_position($max, $interval, -1);
		
		# Obviously, max tick and min tick must not overlap:
		next if $max_Tick <= $min_Tick;
		
		# Count the number of ticks between min_Tick and max_Tick
		# and compute the score:
		my $N_Ticks = ($max_Tick - $min_Tick) / $interval + 1;
		$handicap += $offset_penalty if $offset != 0;
		my $score = (abs($N_Ticks - 5) + $handicap)**2;
		
		# If it's the best score, use it:
		if ($score < $best_score) {
			$best_score = $score;
			# Add 0.1 to N_Ticks to avoid rounding dilemas:
			$best_Ticks = zeroes($N_Ticks + 0.1)->xlinvals($min_Tick, $max_Tick);
			$best_interval = $interval;
			$best_scaled_interval = $scaled_interval;
		}
	}
	
	# Construct the minor tick marks:
	my $tick_interval;
	if ($best_scaled_interval eq '0.3'
			or $best_scaled_interval eq '3'
			or $best_scaled_interval eq '0.6'
	) {
		$tick_interval = $best_interval / 6;
	}
	elsif (($best_scaled_interval eq '0.25'
			or $best_scaled_interval eq '0.5'
			or $best_scaled_interval eq '2.5'
			) and $best_offset == 0
	) {
		$tick_interval = $best_interval / 5;
	}
	else {
		$tick_interval = $best_interval / 4;
	}
	my $min_tick = _adjusted_position($min, $tick_interval, $best_offset, 1);
	my $max_tick = _adjusted_position($max, $tick_interval, $best_offset, -1);
	my $N_ticks = ($max_tick - $min_tick) / $tick_interval + 1.1;
	# Add 0.1 to escape rounding trouble.
	my $ticks = zeroes($N_ticks)->xlinvals($min_tick, $max_tick);

	# finally, set up zero values that make sense:
	$best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min)) .= 0;
	$ticks->where(abs($ticks) * 1e10 < ($max - $min)) .= 0;
	return ($best_Ticks, $ticks);

=cut

}

sub Prima::Drawable::graph {
	my ($canvas, @args) = @_;
	my %options = ();
	if (ref($args[-1]) eq 'HASH') {
		%options = %{pop @args};
	}
	
	# Get the scaling parameter:
	my $scaling = 'linear';
	$scaling = $options{scaling} if exists $options{scaling};

	# The rest of @args are the datasets, together with their arguments
	# that go to polyline.
	
	# Process the min/max specifications:
	my ($xmin, $ymin, $xmax, $ymax) = @options{qw(xmin ymin xmax ymax)};
	
	# Get the min/max from the data if it's not already specified
	if (not defined $xmin or not defined $ymin or not defined $xmax
			or not defined $ymax) {
		foreach(@args) {
			if (not $options{xmin}) {
				$xmin = $_->[0]->min if (not defined $xmin or $_->[0]->min < $xmin);
			}
			if (not $options{ymin}) {
				$ymin = $_->[1]->min if (not defined $ymin or $_->[1]->min < $ymin);
			}
			if (not $options{xmax}) {
				$xmax = $_->[0]->max if (not defined $xmax or $_->[0]->max > $xmax);
			}
			if (not $options{ymax}) {
				$ymax = $_->[1]->max if (not defined $ymax or $_->[1]->max > $ymax);
			}
		}
	}
	
	if ($scaling =~ /logx/) {
		croak("Minimum and maximum values must be strictly positive for logarithmic scaling")
			unless ($xmin > 0 and $xmax > 0);
		# Rescale the min/max
		$xmin = log($xmin) / log(10);
		$xmax = log($xmax) / log(10);
		
		# Rescale the data:
		foreach (@args) {
			$_->[0] = log($_->[0]) / log 10;
		}
	}
	if ($scaling =~ /logx?y/) {
		croak("Minimum and maximum values must be strictly positive for logarithmic scaling")
			unless ($ymin > 0 and $ymax > 0);
		# Rescale the min/max
		$ymin = log($ymin) / log(10);
		$ymax = log($ymax) / log(10);
		
		# Rescale the data:
		foreach (@args) {
			$_->[1] = log($_->[1]) / log 10;
		}
	}
	
	# Get the viewport
	my @viewport = (0.1, 0.1, 0.9, 0.9);
	if (exists $options{viewport}) {
		croak("Viewport must be a four-element anonymous array")
			unless (ref($options{viewport})
					and ref($options{viewport}) eq 'ARRAY'
					and @{$options{viewport}} == 4);
		foreach (@{$options{viewport}}) {
			croak("Viewport boundaries must be between 0 and 1")
				unless $_ >= 0 and $_ <= 1;
		}
		croak("Viewport's bottom must be lower than the top")
			if $options{viewport}->[1] >= $options{viewport}->[3];
		croak("Viewport's left edge must be to the left of the right edge")
			if $options{viewport}->[0] >= $options{viewport}->[2];
	
		# If we're here, then all is well.
		@viewport = @{$options{viewport}};
	}
	# Conver the viewport to pixels and apply it:
	my ($x_size, $y_size) = $canvas->size;
	$viewport[0] *= $x_size;
	$viewport[1] *= $y_size;
	$viewport[2] *= $x_size;
	$viewport[3] *= $y_size;
	$canvas->clipRect(@viewport);
	
	# Compute the extent of the viewport in pixels:
	my $x_extent = $viewport[2] - $viewport[0];
	my $y_extent = $viewport[3] - $viewport[1];
	
	# Save the canvas's current properties that may be overwritten
	# by the polylines calls:
	my @to_backup = qw(color backColor linePattern lineWidth lineJoin
			lineEnd rop rop2);
	my %backups = map {$_ => $canvas->$_} (@to_backup);
	
	# Draw the data
	foreach(@args) {
		# Unpack the drawing specs:
		my ($xs, $ys, %props) = @$_;
		# Rescale x and y to pixel values:
		my $x_to_plot = rescale_to_pixels($xs, $xmin, $xmax, $x_extent, $viewport[0]);
		my $y_to_plot = rescale_to_pixels($ys, $ymin, $ymax, $y_extent, $viewport[1]);
		# Call the plotting routine:
		$canvas->pdl_polylines($x_to_plot, $y_to_plot, %props);
		# Reset the parameters in case they were changed:
		$canvas->set(%backups);
	}
	
	# Reset the clipping rectangle to the full canvas and draw a box
	# around the data:
	$canvas->clipRect(0, 0, $x_size, $y_size);
	$canvas->rectangle(@viewport);
	
	# Draw the tick marks:
	my ($xTicks, $xticks, $yTicks, $yticks);
#	if ($scaling =~ /logx/) {
#		($xTicks, $xticks) = compute_log_ticks($xmin, $xmax);
#	}
#	else {
		($xTicks, $xticks) = compute_linear_ticks($xmin, $xmax);
#	}
#	if ($scaling =~ /logx?y/) {
#		($yTicks, $yticks) = compute_log_ticks($ymin, $ymax);
#	}
#	else {
		($yTicks, $yticks) = compute_linear_ticks($ymin, $ymax);
#	}

	my $xticks_pixels = rescale_to_pixels($xticks, $xmin, $xmax, $x_extent, $viewport[0]);
	my $xTicks_pixels = rescale_to_pixels($xTicks, $xmin, $xmax, $x_extent, $viewport[0]);
	my $yticks_pixels = rescale_to_pixels($yticks, $ymin, $ymax, $y_extent, $viewport[1]);
	my $yTicks_pixels = rescale_to_pixels($yTicks, $ymin, $ymax, $y_extent, $viewport[1]);

	# Draw the tick marks:
	my $top_bottom = pdl($viewport[1], $viewport[3])->transpose;
	my $left_right = pdl($viewport[0], $viewport[2])->transpose;
	my $tick_length = sqrt($x_size);
	$tick_length = sqrt($y_size) if sqrt($y_size) < $tick_length;
	$tick_length *= 0.8;
	my $Tick_size = pdl($tick_length, -$tick_length)->transpose;
	my $tick_size = $Tick_size / 2;
	$canvas->pdl_lines($xticks_pixels, $top_bottom, $xticks_pixels, $top_bottom + $tick_size);
	$canvas->pdl_lines($xTicks_pixels, $top_bottom, $xTicks_pixels, $top_bottom + $Tick_size);
	$canvas->pdl_lines($left_right, $yticks_pixels, $left_right + $tick_size, $yticks_pixels);
	$canvas->pdl_lines($left_right, $yTicks_pixels, $left_right + $Tick_size, $yTicks_pixels);
	
	# Draw the tick labels
	for (my $i = 0; $i < $xTicks->nelem; $i++) {
		my $x = $xTicks_pixels->at($i);
		my $string = sprintf("%1.8g", $xTicks->at($i));
		$canvas->draw_text($string
			, $x-80, 0, $x+80, $viewport[1] - 2*log($y_size)
			, dt::Top | dt::Center
			);
	}
	for (my $i = 0; $i < $yTicks->nelem; $i++) {
		my $y = $yTicks_pixels->at($i);
		my $string = sprintf("%1.8g", $yTicks->at($i));
		$canvas->draw_text($string
			, 0, $y-80, $viewport[0] - 2*log($x_size), $y+80
			, dt::Right | dt::VCenter
			);
	}
	
	# Position the labels
	if ($options{xlabel}) {
		$canvas->draw_text($options{xlabel}
			, 0, 0, $x_size, $viewport[1]/2
			, dt::Center | dt::Top
			);
	}
	if ($options{ylabel}) {
		$canvas->font(direction => 90);
		$canvas->draw_text($options{ylabel}
			, 0, 0, $viewport[0]/2, $y_size
			, dt::VCenter | dt::Right
		);
		$canvas->font(direction => 0);
	}
}

# For now, just a simple wrapper around the graph function.
sub Prima::Drawable::bin {
	my $self = shift;
	my $args = {};
	my $total_y_max = 0;
	$args = pop @_ if (ref($_[-1]) and ref($_[-1]) eq 'HASH');
	my @to_send;
	foreach (@_) {
		# Assume for now that all the bin centers are the same size:
		my ($xvals, $yvals) = @$_;
		my $bin_width = $xvals->at(1) - $xvals->at(0);
		my $first_column = $xvals - $bin_width / 2;
		my $third_column = $xvals + $bin_width / 2;
		my $xs = cat($first_column, $first_column, $third_column)
				->transpose->flat->append($third_column->at(-1));
		
		$first_column = $yvals->zeroes;
		my $second_column = $yvals;
		my $ys = cat($first_column, $second_column, $second_column)->transpose->flat->append(0);
		
		$total_y_max = $ys->max if $total_y_max < $ys->max;
		
		push @to_send, [$xs, $ys];
	}
	$args->{ymax} = $total_y_max * 1.05 unless defined $args->{ymax};
	$self->graph(@to_send, $args);
}

sub get_min_max_for {
	my ($first, $second) = @_;
	return ($first, $second) if $first < $second;
	return ($second, $first);
}

=head2 endow

Adds a number of properties and event handles to a widget to make it a
navigable graph.

=cut

use Time::HiRes qw(gettimeofday tv_interval);

# Turns a widget into a graph
sub endow {
	my ($self, $x, $y, $xlabel, $ylabel, $title, $style) = @_;
	my ($xmin, $xmax) = $x->minmax;
	my ($ymin, $ymax) = $y->minmax;
	my ($clickx, $clicky);
	$style //= 'graph';
	my @previous_click = (0,0);
	my $op = \&print_coords;
	# working here - change the scaling with a pop-up menu
	my $scaling = 'linear';
	my $drag_button = 0;
	my @right_button_xy = ();
	my $previous_click_time = [gettimeofday];

	$self->onPaint(sub {
		# wipe and replot:
		$self->clear;
		my $x_to_plot = $x;
		my $y_to_plot = $y;
		# For performance, try to only work with the data that is
		# within the window if the dataset is huge:
		if ($x->nelem > 10_000) {
			($x_to_plot, $y_to_plot)
				= where($x_to_plot, $y_to_plot, ($xmin < $x_to_plot)
					& ($x_to_plot < $xmax));
		}
		# If the dataset is still huge, downsample it:
		my $starting_x = $x_to_plot;
		my $starting_y = $y_to_plot;
		for (my $i = 2; $x_to_plot->nelem > 5_000; $i++) {
			$x_to_plot = $starting_x(0:-1:$i);
			$y_to_plot = $starting_y(0:-1:$i);
		}
#		while ($x_to_plot->nelem > 5_000) {
#			$x_to_plot = $x_to_plot(0:-1:2);
#			$y_to_plot = $y_to_plot(0:-1:2);
#		}
		my @args = ([$x_to_plot, $y_to_plot], {
			xmin => $xmin,
			xmax => $xmax,
			ymin => $ymin,
			ymax => $ymax,
			scaling => $scaling,
			xlabel => $xlabel,
			ylabel => $ylabel,
			});
		$self->graph(@args) if $style eq 'graph';
		$self->bin(@args) if $style eq 'bin';
		my ($width, $height) = $self->size;
		$self->draw_text($title, 0, $height - 20, $width, $height
			, dt::Center | dt::Bottom);
		
		if (@right_button_xy) {
			# Draw a zoom rectangle:
			$self->rectangle($clickx, $clicky, @right_button_xy);
		}
	});
	$self->backColor(cl::White);
	$self->onMouseWheel( sub {
		my (undef, undef, $x, $y, $dir) = @_;
		my ($x_max, $y_max) = $self->size;
		if ($x > $x_max/10) {
			my $xrange = $xmax - $xmin;
			if ($dir > 0) {
				$xmin += $xrange/10;
				$xmax -= $xrange/10;
			}
			else {
				$xmin -= $xrange/10;
				$xmax += $xrange/10;
			}
		}
		if ($y > $y_max/10) {
			my $yrange = $ymax - $ymin;
			if ($dir > 0) {
				$ymin += $yrange/10;
				$ymax -= $yrange/10;
			}
			else {
				$ymin -= $yrange/10;
				$ymax += $yrange/10;
			}
		}
		$self->notify("Paint");
	});
	$self->onMouseDown( sub {
		(undef, $drag_button, undef, $clickx, $clicky) = @_;
	});
	$self->onMouseMove( sub {
		my (undef, undef, $x, $y) = @_;
		return unless defined $clickx;

		if ($drag_button == mb::Left) {
			my ($x_max_pixel, $y_max_pixel) = $self->size;
			my $dx_pixel = $x - $clickx;
			my $dy_pixel = $y - $clicky;
			
			# Store the new values
			$clickx = $x;
			$clicky = $y;
			
			# Rescale dx and dy to coordinates
			my $dx = $dx_pixel / (0.8*$x_max_pixel) * ($xmax - $xmin);
			my $dy = $dy_pixel / (0.8*$y_max_pixel) * ($ymax - $ymin);
			
			# Adjust the x and y values:
			$xmax -= $dx;
			$xmin -= $dx;
			$ymin -= $dy;
			$ymax -= $dy;
		}
		else {
			@right_button_xy = ($x, $y);
		}
		$self->notify("Paint");
	});
	$self->onMouseClick( sub {
		my (undef, $button, undef, $mouse_x, $mouse_y, $double_click) = @_;
		if ($double_click and $button == mb::Left) {
			my ($x_max_pixel, $y_max_pixel) = $self->size;
			# Is this an auto-scale double-click?
			if ($mouse_x < $x_max_pixel / 10) {
				($ymin, $ymax) = $y->where(($xmin < $x) & ($x < $xmax))->minmax;
				$self->notify("Paint");
			}
			elsif ($mouse_y < $y_max_pixel / 10) {
				($xmin, $xmax) = $x->minmax;
				$self->notify("Paint");
			}
			else {
				# print location, and position relative to previous location
				# store current real coordinates.
				my $actualx = $mouse_x / (0.8*$x_max_pixel) * ($xmax - $xmin) + $xmin;
				my $actualy = $mouse_y / (0.8*$y_max_pixel) * ($ymax - $ymin) + $ymin;
				print "Coordinate ($actualx, $actualy) is (", $actualx - $previous_click[0]
					, ", ", $actualy - $previous_click[1], ") from the last click\n";
				
				@previous_click = ($actualx, $actualy);
			}
		}
	});
	$self->onMouseUp( sub {
		if (defined $drag_button and $drag_button == mb::Right) {
			# Perform the zoom they want
			my ($left_pixel, $right_pixel)
				= get_min_max_for($clickx, $right_button_xy[0]);
			my ($bottom_pixel, $top_pixel)
				= get_min_max_for($clicky, $right_button_xy[1]);
			
			# Avoid double-click trouble:
			if ($right_pixel - $left_pixel > 3
				or $top_pixel - $bottom_pixel > 3) {
				
				my ($width, $height) = $self->size;
				
				# Rescale the coordinates:
				my $xrange = $xmax - $xmin;
				my $old_xmin = $xmin;
				my $yrange = $ymax - $ymin;
				my $old_ymin = $ymin;
				$xmin = ($left_pixel - $width/10) / ($width * 0.8) * $xrange + $old_xmin;
				$xmax = ($right_pixel - $width/10) / ($width * 0.8) * $xrange + $old_xmin;
				$ymin = ($bottom_pixel - $height/10) / ($height  * 0.8) * $yrange + $old_ymin;
				$ymax = ($top_pixel - $height/10) / ($height * 0.8) * $yrange + $old_ymin;
				
				# Redraw it
				$self->notify("Paint");
			}
		}
		
		undef $drag_button;
		@right_button_xy = ();
		undef $clickx;
		undef $clicky;
	});
	
	$self->{new_data} = sub {
		($x, $y, $title) = @_;
		$self->notify("Paint");
	};
	$self->{get_current_ys} = sub {
		return where($y, ($xmin < $x) & ($x < $xmax) & ($ymin < $y)
								& ($y < $ymax));
	};
	$self->{get_x_bounds} = sub {
		return ($xmin, $xmax);
	};
}


