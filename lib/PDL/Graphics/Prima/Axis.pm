use strict;
use warnings;

# Here's a package to handle the axes for me:
package PDL::Graphics::Prima::Axis;

use PDL::Graphics::Prima::Limits;
use PDL::Graphics::Prima::Scaling;
use Carp;

use PDL;
use Prima;
our @ISA = qw(Prima::Component);

#################
# Notifications #
#################
# Add a new notification_type for each of the notifications just defined.
{
	# Keep the notifications hash in its own lexically scoped block so that
	# other's can't mess with it (at least, not without using PadWalker or some
	# such).
#	my %new_notifications = map {"Change$_" => nt::Default} qw(Bounds Scaling View);
	my %notifications = (
		%{Prima::Component-> notification_types()},
		map { ("Change$_" => nt::Default) } qw(Bounds Scaling View)
	);
	
	sub notification_types { return \%notifications }
}

sub repaint_parent {
	my $axis = shift;
	$axis->owner->notify('Replot')
}
*on_changebounds = \&repaint_parent;
*on_changescaling = \&repaint_parent;
*on_changeview = \&repaint_parent;

###################
# Default Profile #
###################

sub profile_default {
	my %def = %{$_[ 0]-> SUPER::profile_default};

	return {
		%def,
		# default properties go here
		scaling => sc::Linear,
		min => lm::Auto,
		max => lm::Auto,
		viewMin => 0.1,
		viewMax => 0.9,
	};
}

# This initializes self's data from the profile:
sub init {
	my $self = shift;
	my %profile = $self->SUPER::init(@_);
	foreach ( qw(scaling viewMin viewMax) ) {
		$self->{$_} = $profile{$_};
	}
	
	# Process the minima and maxima.
	if ($profile{min} == lm::Auto) {
		$self->{minAuto} = 1;
		$self->{minValue} = 1;
	}
	else {
		croak ("min value must be a real number, lm::Auto, or lm::Hold")
			if $profile{min} != $profile{min};
		$self->{minValue} = $profile{min};
		$self->{minAuto} = 0;
	}
	if ($profile{max} == lm::Auto) {
		$self->{maxAuto} = 1;
		$self->{maxValue} = 2;
	}
	else {
		croak ("max value must be a real number, lm::Auto, or lm::Hold")
			if $profile{max} != $profile{max};
		$self->{maxAuto} = 0;
		$self->{maxValue} = $profile{max};
	}
	
	
	# I am fairly certain that I don't need to set this to a meaningful value
	# here since the graph widget will be Sized when it is first displayed. The
	# onSize callback for the graph widget is set to change the x- and y-axis
	# pixel extents, so they will be meaningful before any calculations or
	# drawing takes place.
	$self->{pixel_extent} = 10;
}


# Simply changes the internal value; does not issue a redraw. The pixel_extent
# data contains the full width or height of the widget, in pixels.
sub pixel_extent {
	if (@_ == 1) {
		return $_[0]->{pixel_extent};
	}
	
	my ($self, $new_extent) = @_;

	$self->{pixel_extent} = $new_extent;
	$self->recompute_auto;
}

=head1 Properties

=head2 min, max

Gets/sets the the individual extrema. The return value depends upon the calling
context. If requested in scalar context, you simply get the current calculated
extreme value. If requested in list context, you get two return values,
the first being the extremum and the second being a boolean value
indicating whether or not the Auto flag is set.

=cut

sub recompute_min_auto {
	my ($self) = @_;
	$self->{minAuto} = 1;
	my $extremum_name = $self->name . 'min';

	my ($min, $padding) = $self->owner->compute_data_extremum($extremum_name);

	# Only change things if a defined value was returned for the minimum.
	# (Undefined minima can be returned if there is no data, for example.)
	if (defined $min) {
		# Make sure we don't have defined but useless values:
		{
			no warnings 'numeric';
			confess("Min calculated as nan") if $min != $min;
			if ($min+1 == $min) {
				confess("Min calculated as inf") if $min > 0;
				confess("Min calculated as -inf");
			}
		}
		my $max = $self->{maxValue};
		# call the scaling object's methods for computing the real min/max, with
		# the padding taken into account:
		($min, $max) = $self->min_max_with_padding($min, $max
			, min_padding => $padding
			, max_padding => 0);
		die "Bad min calculation" if $min != $min;
		$self->{minValue} = $min;
	}
}

sub recompute_max_auto {
	my ($self) = @_;
	$self->{maxAuto} = 1;
	my $extremum_name = $self->name . 'max';

	my ($max, $padding) = $self->owner->compute_data_extremum($extremum_name);

	# Only change things if a defined value was returned for the maximum.
	# (Undefined maxima can be returned if there is no data, for example.)
	if (defined $max) {
		# Make sure we don't have defined but useless values:
		{
			no warnings 'numeric';
			confess("Max calculated as nan") if $max != $max;
			if ($max+1 == $max) {
				confess("Max calculated as inf") if $max > 0;
				confess("Max calculated as -inf");
			}
		}
		my $min = $self->{minValue};
		# call the scaling object's methods for computing the real max/max, with
		# the padding taken into account:
		($min, $max) = $self->min_max_with_padding($min, $max
			, max_padding => $padding
			, min_padding => 0);
		die "Bad max calculation" if $max != $max;
		$self->{maxValue} = $max;
	}
}

sub recompute_auto {
	my ($self) = @_;
	# Perform the calculation if either value is auto:
#	if ($self->{minAuto} or $self->{maxAuto}) {
#		my ($min, $max) = $self->owner->compute_min_max_for($self->name);
#		
#	}
	$self->recompute_min_auto if ($self->{minAuto});
	$self->recompute_max_auto if ($self->{maxAuto});
}

# Accessor functions. There are a bunch of private functions that do
# exactly what the public functions would have done, except that they
# do not issue a notification.
sub _min {
	my ($self, $new_value) = @_;
	my ($value, $is_auto) = ($self->{minValue}, $self->{minAuto});

	if (not defined $new_value) {
		# do nothing
	}
	elsif ($new_value == lm::Auto) {
		$self->recompute_min_auto;
	}
	elsif ($new_value == lm::Hold) {
		# Hold means they want to keep the current bounds, so stop
		# auto-scaling:
		$self->{minAuto} = 0;
	}
	else {
		# Set the min to the new value:
		$self->{minAuto} = 0;
		croak("Invalid minimum value: $@")
			unless ($self->{scaling}->is_valid_extremum($new_value));
		$self->{minValue} = $new_value;
	}
	
	return $value unless wantarray;
	return ($value, $is_auto);
}
{
	# Create the min function without issuing a redefinition warning
	no warnings 'redefine';
	sub min {
		# If no new value, just calling to get the current values:
		my $self = shift;
		my ($value, $is_auto) = $self->_min(@_);
		$self->notify('ChangeBounds') if (@_);
		
		return $value unless wantarray;
		return ($value, $is_auto);
	}
}
sub _max {
	my ($self, $new_value) = @_;
	my ($value, $is_auto) = ($self->{maxValue}, $self->{maxAuto});
	
	if (not defined $new_value) {
		# do nothing
	}
	elsif ($new_value == lm::Auto) {
		$self->recompute_max_auto;
	}
	elsif ($new_value == lm::Hold) {
		# Hold means they want to keep the current bounds, so stop
		# auto-scaling:
		$self->{maxAuto} = 0;
	}
	else {
		# Set the max to the new value:
		croak("Invalid maximum value: $@")
			unless ($self->{scaling}->is_valid_extremum($new_value));
		$self->{maxAuto} = 0;
		$self->{maxValue} = $new_value;
	}
	
	return $value unless wantarray;
	return ($value, $is_auto);
}
{
	# Create the max function without issuing a redefinition warning
	no warnings 'redefine';
	sub max {
		# If no new value, just calling to get the current values:
		my $self = shift;
		my ($value, $is_auto) = $self->_max(@_);
		$self->notify('ChangeBounds') if (@_);
		
		return $value unless wantarray;
		return ($value, $is_auto);
	}
}

=head2 minmax

Pair accessor. You can set the min/max values in one shot with this function,
and you will get a two-element list if you call it as a getter. For example:

 my $piddle = get_data;
 $graph_widget->x->minmax($piddle->minmax);
 
 # ...
 
 print "The x min/max values are ", join(', ', $graph_widget->x->minmax), "\n";


=cut

# working here - this could be made more efficient when both the min and the
# max end up being auto-scaling:
{
	# Create the minmax function without issuing a redefinition warning
	no warnings 'redefine';

	sub minmax {
		my @minmax = ($_[0]->{minValue}, $_[0]->{maxValue});
		if (@_ > 1) {
			$_[0]->_min($_[1]);
			$_[0]->_max($_[2]);
			$_[0]->notify('ChangeBounds');
		}
		return @minmax;
	}
}

# This computes the adjusted minimum and maximum for the given pixel padding:
sub min_max_with_padding {
	# Unpack the arguments:
	my ($self, $min, $max, %args) = @_;
	{
		no warnings 'numeric';
									# nan check         inf check
		confess("Min must be real") if ($min != $min or $min+1 == $min);
		confess("Max must be real") if ($max != $max or $max+1 == $max);
	}
	my ($min_padding, $max_padding) = @args{qw(min_padding max_padding)};
	
	# Determine the pixel extent (width or height) of the data as if the pixel
	# extent of the plotting region had the padding cut out of it:
	my $virtual_pixel_extent
		= $self->{pixel_extent} * ($self->viewMax - $self->viewMin)
			- $max_padding - $min_padding;

	# Compute the relative positions of the edges of the padding, given the
	# virtual (reduced) pixel extent:
	my $relative_padding_min = -$min_padding / $virtual_pixel_extent;
	my $relative_padding_max = 1 + $max_padding / $virtual_pixel_extent;
	
	# Perform the inverse scaling transform to get the real numbers
	# corresponding to those relative values:
	my $new_min = $self->scaling->inv_transform($min, $max, $relative_padding_min);
	my $new_max = $self->scaling->inv_transform($min, $max, $relative_padding_max);
	
	die "Bad new min from inv_transform"
		#          nan check             inf check
		if ($new_min != $new_min or $new_min+1 == $new_min);
	die "Bad new max from inv_transform"
		#          nan check             inf check
		if ($new_max != $new_max or $new_max+1 == $new_max);
	
	# Return those real numbers:
	return ($new_min, $new_max);
}

# working here - document

sub scaling {
	return $_[0]->{scaling} unless $#_;
	# working here - what if the old limits are invalid with the new scaling,
	# such as negative limits with logarithmic scaling?
	$_[0]->{scaling} = $_[1];
	$_[0]->notify('ChangeScaling');
}


=head2 reals_to_relatives, relatives_to_reals

Converts real values (i.e. numbers in the set of reals, as opposed to the set
of complex numbers, or integers) to their relative pixel positions within the
plot window, where by relative, I mean the result is a number between 0 and 1.
This takes the scaling (logarithmic, linear, etc) into account.

Actually, it can be less than 0 or greater than 1. If you have a real number
that is less than the plot's minimum value, it will have a negative relative
value, and if you have a real number that is greater than the plot's maximum
value, it will have a relative number greater than 1. This is probably better
understood through a few examples.

Suppose your graph has a min/max of 0 and 100. For linear scaling, a value
of 50 would have a relative position of 0.5, a value of 10 would have a relative
position of 0.1, 200 would have a relative position of 2, and -10 would have a
relative position of -0.1. Real numbers that are less than the min get negative


=for details
Note that the internal min/max values are stored in C<minValue> and C<maxValue>.

=cut

sub reals_to_relatives {
	my ($axis, $dataset, $min, $max) = @_;
	$min = $axis->{minValue} unless defined $min;
	$max = $axis->{maxValue} unless defined $max;
	return $axis->{scaling}->transform($min, $max, $dataset);
}

sub relatives_to_reals {
	my ($axis, $dataset, $min, $max) = @_;
	$min = $axis->{minValue} unless defined $min;
	$max = $axis->{maxValue} unless defined $max;
	return $axis->{scaling}->inv_transform($min, $max, $dataset);
}

=head2 pixels_to_relatives, relatives_to_pixels

Converts relative plot positions to their on-widget pixel locations. The
widget's pixel origin is taken to be zero at the lower left corner of the
widget, so this both rescales the numbers and includes the appropriate offset.

=cut

sub pixels_to_relatives {
	my ($axis, $dataset) = @_;
	my $pixel_min = $axis->viewMin * $axis->{pixel_extent};
	my $view_percent = $axis->{viewMax} - $axis->{viewMin};
	return ($dataset - $pixel_min) / ($axis->{pixel_extent} * $view_percent);
}

sub relatives_to_pixels {
	my ($axis, $dataset) = @_;
	my $view_percent = $axis->{viewMax} - $axis->{viewMin};
	return $axis->{pixel_extent} * ($dataset * $view_percent + $axis->{viewMin})
}

=head2 reals_to_pixels, pixels_to_reals

A convenience function to convert real values directly to on-widget pixel
locations. This simply combines the previous two documented functions.

=cut

sub reals_to_pixels {
	my ($axis, $dataset, @args) = @_;
	return $axis->relatives_to_pixels($axis->reals_to_relatives($dataset, @args));
}

sub pixels_to_reals {
	my ($axis, $dataset, @args) = @_;
	return $axis->relatives_to_reals($axis->pixels_to_relatives($dataset), @args);
}

=head2 viewMin, viewMax, viewMinMax

Gets or sets the minimum and maximum values of the axis.

=cut

sub viewMin {
	return $_[0]->{viewMin} unless $#_;
	my ($axis, $min) = @_;
	my $to_return = $axis->{viewMin};
	
	# Min and max cannot be the same:
	croak('New minimum is equal to the old maximum, which is not allowed')
		if $min == $axis->{viewMax};
	
	# Set the new min:
	$axis->{viewMin} = $min;
	# Swap the min/max if necessary:
	($axis->{viewMin}, $axis->{viewMax}) = ($axis->{viewMax}, $axis->{viewMin})
		if $axis->{viewMax} < $axis->{viewMin};
	
	
	# Check if this min is greater than the max; if so, swap them:
	$axis->notify('ChangeView');
	return $to_return;
}

sub viewMax {
	return $_[0]->{viewMax} unless $#_;
	my ($axis, $max) = @_;
	my $to_return = $axis->{viewMax};

	# Min and max cannot be the same:
	croak('New maximum is equal to the old minimum, which is not allowed')
		if $max == $axis->{viewMin};
	
	# Set the new max:
	$axis->{viewMax} = $max;
	# Swap the min/max if necessary:
	($axis->{viewMin}, $axis->{viewMax}) = ($axis->{viewMax}, $axis->{viewMin})
		if $axis->{viewMax} < $axis->{viewMin};

	$axis->notify('ChangeView');
	return $to_return;
}

sub viewMinMax {
	return ($_[0]->{viewMin}, $_[0]->{viewMax}) unless $#_;
	my ($axis, $min, $max) = @_;
	my (@to_return) = ($axis->{viewMin}, $axis->{viewMax});
	
	croak('New min and max have the same value')
		if ($min == $max);
	
	# Swap the min/max values if necessary:
	($min, $max) = ($max, $min) if $max < $min;
	# Set the new values:
	$axis->{minView} = $min;
	$axis->{maxView} = $max;
	
	$axis->notify('ChangeView');
	return @to_return;
}

sub draw {
	my ($axis, $canvas) = @_;
	
	# Get the locations for the major and minor ticks:
	my ($Ticks, $ticks) = $axis->{scaling}->compute_ticks($axis->minmax);
	
	# Rescale to pixels:
	my $ticks_pixels = $axis->reals_to_pixels($ticks);
	my $Ticks_pixels = $axis->reals_to_pixels($Ticks);
	
	# At some point, make this call the function $axis->{drawing_function} or
	# some such. In the meantime, just draw it on the edges:
	my ($canv_width, $canv_height) = $canvas->size;
	my $tick_length = 0.8 * sqrt($canv_width < $canv_height ? $canv_height : $canv_width);
	my $Tick_size = pdl($tick_length, -$tick_length)->transpose;
	my $tick_size = $Tick_size / 2;
	
	my $top_bottom = pdl($canvas->y->viewMinMax)->transpose * $canv_height;
	my $left_right = pdl($canvas->x->viewMinMax)->transpose * $canv_width;
	
	if ($axis->name eq 'x') {
		$canvas->pdl_lines($ticks_pixels, $top_bottom, $ticks_pixels, $top_bottom + $tick_size);
		$canvas->pdl_lines($Ticks_pixels, $top_bottom, $Ticks_pixels, $top_bottom + $Tick_size);

		# Figure out the top of the axis labels:
		my $label_top = $canv_height * $canvas->y->viewMin - 2*log($canv_height);
		for (my $i = 0; $i < $Ticks->nelem; $i++) {
			my $x = $Ticks_pixels->at($i);
			my $string = sprintf("%1.8g", $Ticks->at($i));
			$canvas->draw_text($string, $x-80, 0, $x+80, $label_top, dt::Top | dt::Center);
		}
		
		# Draw lines on the top/bottom:
		$canvas->pdl_lines($left_right->at(0,0), $top_bottom, $left_right->at(0,1), $top_bottom);
	}
	else {
		$canvas->pdl_lines($left_right, $ticks_pixels, $left_right + $tick_size, $ticks_pixels);
		$canvas->pdl_lines($left_right, $Ticks_pixels, $left_right + $Tick_size, $Ticks_pixels);

		# Figure out the right edge of the axis labels:
		my $label_right = $canv_width * $canvas->x->viewMin - 2*log($canv_width);
		for (my $i = 0; $i < $Ticks->nelem; $i++) {
			my $y = $Ticks_pixels->at($i);
			my $string = sprintf("%1.8g", $Ticks->at($i));
			$canvas->draw_text($string, 0, $y-80, $label_right, $y+80, dt::Right | dt::VCenter);
		}
		
		# Draw lines on the left/right:
		$canvas->pdl_lines($left_right, $top_bottom->at(0,0), $left_right, $top_bottom->at(0,1));
	}
}

1;
