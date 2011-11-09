use strict;
use warnings;

# Defines the lm (limits) package:
package lm;
my $inf;
BEGIN {
	use PDL::Lite;
	$inf = -PDL->pdl(0)->log->at(0);
}

use constant Auto => $inf;
use constant Hold => -$inf;

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
	my %notifications = (
		%{Prima::Component-> notification_types()},
		map { ("Change$_" => nt::Default) } qw(Bounds Scaling Label)
	);
	
	sub notification_types { return \%notifications }
}

sub repaint_parent {
	my $axis = shift;
	$axis->owner->notify('Replot')
}
*on_changebounds = \&repaint_parent;
*on_changescaling = \&repaint_parent;
*on_changelabel = \&update_edges;

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
		label => '',
	};
}

# This initializes self's data from the profile:
sub init {
	my $self = shift;
	my %profile = $self->SUPER::init(@_);
	$self->{scaling} = $profile{scaling};
	
	# I will be using M-widths and heights for various calculations:
	my $points = $self->owner->get_text_box('M');
	my $em_width = $self->{em_width} = $points->[4] - $points->[0];
	my $em_height = $self->{em_height} = $points->[1] - $points->[3];
	
	# The edge requirements depend on whether we're working with an x or a
	# y axis. Set the non-changing parts here and then set the changing
	# parts by calling _label. The four elements are the left edge, the
	# bottom edge, the right edge, and the top edge:
	if ($self->name eq 'x') {
		$self->{edge_requirements} = [$em_height, 0, $em_height, 0];
	}
	else {
		$self->{edge_requirements} = [0, $em_height, 0, $em_height];
	}
	# 're'set the label to force the last set of calculations:
	$self->_label($profile{label});
	
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
}

# low-level setter; does not notify
sub _label {
	my ($self, $label) = @_;
	
	# Consider recomputing these each time, or setting an accessor for font
	# settings:
	my $em_width = $self->{em_width};
	my $em_height = $self->{em_height};
	
	# Is the label blank or undefined?
	if (not defined $label or $label eq '') {
		# Remove the label in that case:
		$self->{label} = '';
		
		# Set the edge requirements based on axis type:
		if ($self->name eq 'x') {
			# Set bottom edge requirement:
			$self->{edge_requirements}->[1] = 1.5 * $em_height;
		}
		else {
			# Set left edge requirement:
			$self->{edge_requirements}->[0] = 4 * $em_width;
		}
	}
	else {
		# Set the label:
		$self->{label} = $label;
		
		# Set the edge requirements absed on axis type:
		if ($self->name eq 'x') {
			# Set bottom edge requirement:
			$self->{edge_requirements}->[1] = 3 * $em_height;
		}
		else {
			# Set left edge requirement to accomodate the tick labels and
			# the axis label:
			$self->{edge_requirements}->[0]
				= 4 * $em_width + 1.5 * $em_height;
		}
	}
}

sub label {
	return $_[0]->{label} if @_ == 1;
	my ($self, $label) = @_;
	return if $label eq $self->{label};
	$self->_label($label);
	$self->notify('ChangeLabel');
}

# Return the space needed to draw axis labels and tick labels.
sub get_edge_requirements {
	return @{$_[0]->{edge_requirements}};
}

=head2 update_edges

Updates the internal storage of the edge data and initiates a recomputation
of the autoscaling, if appropriate. This is usually triggered by a window
resize or a new or modified dataset.

=cut

sub update_edges {
	my ($self) = @_;
	return if $self->{initializing};
	
	# Get and store the new edges:
	my @edges = $self->owner->get_edge_requirements;
	if ($self->name eq 'x') {
		$self->{lowerEdge} = $edges[0];
		$self->{edgeWidth} = $self->owner->width - $edges[2] - $edges[0];
	}
	else {
		$self->{lowerEdge} = $edges[1];
		$self->{edgeWidth} = $self->owner->height - $edges[3] - $edges[1];
	}
	
	# All done, unless we need to do rescaling, too:
	return unless $self->{minAuto} or $self->{maxAuto};
	
	my ($min, $max) = $self->owner->compute_min_max_for($self->name);
	
	# Weird edge case, do nothing if undef was returned:
	return unless defined $min and defined $max;
	
	$self->{minValue} = $min if $self->{minAuto};
	$self->{maxValue} = $max if $self->{maxAuto};
}

=head1 Properties

=head2 min, max

Gets/sets the the individual extrema. The return value depends upon the calling
context. If requested in scalar context, you simply get the current calculated
extreme value. If requested in list context, you get two return values,
the first being the extremum and the second being a boolean value
indicating whether or not the Auto flag is set.

=cut

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
		$self->{minAuto} = 1;
		$self->update_edges;
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
		$self->{maxAuto} = 1;
		$self->update_edges;
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

Note that if you are setting both the min and the max to autoscaling, 
calling minmax(lm::Auto, lm::Auto) is faster than calling min(lm::Auto)
followed by max(lm::Auto).

=cut

# working here - eventually, cache the min and max calculations

{
	# Create the minmax function without issuing a redefinition warning
	no warnings 'redefine';

	sub minmax {
		my @minmax = ($_[0]->{minValue}, $_[0]->{maxValue});
		if (@_ > 1) {
			my ($self, $min, $max) = @_;
			# Handle autoscaling specially. When both autoscale, the
			# recompute should only happen once:
			if ($min == lm::Auto and $max == lm::Auto) {
				$self->{minAuto} = 1;
				$self->{maxAuto} = 1;
				$self->update_edges;
			}
			# otherwise call the individual min/max functions:
			else {
				$self->_min($min);
				$self->_max($max);
			}
			$self->notify('ChangeBounds');
		}
		return @minmax;
	}
}

=head2 scaling

Gets or returns the axis' scaling object. You can change the scaling using
this example with something like this:

 # Switch to logarithmic scaling:
 $widget->x->scaling(sc::Log);

Note, however, that some scalings allow values that are not permissible in
others. For example, Linear scaling allows negative values but Logarithmic
scaling does not. At the moment, if you try to switch to Logarithmic scaling
without ensuring that the current min and max are positive, this will die
telling you that negative values are not allowed.

=cut

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
	return ($dataset - $axis->{lowerEdge}) / $axis->{edgeWidth};
}

sub relatives_to_pixels {
	my ($axis, $dataset) = @_;
	return $axis->{edgeWidth} * $dataset + $axis->{lowerEdge};
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

=head2 draw

Draws the axis, including the bounding box, ticks, and tick labels

=cut


sub draw {
	my ($axis, $canvas, $clip_left, $clip_bottom, $clip_right, $clip_top) = @_;
	
	# Get the locations for the major and minor ticks:
	my ($Ticks, $ticks) = $axis->{scaling}->compute_ticks($axis->minmax);
	
	# Rescale to pixels:
	my $ticks_pixels = $axis->reals_to_pixels($ticks);
	my $Ticks_pixels = $axis->reals_to_pixels($Ticks);
	
	# At some point, make this call the function $axis->{drawing_function} or
	# some such. In the meantime, just draw it on the edges:
	# working here - make this prettier
	my ($canv_width, $canv_height) = $canvas->size;
	my $tick_length = 8; #0.8 * sqrt($canv_width < $canv_height ? $canv_height : $canv_width);
	my $Tick_size = pdl($tick_length, -$tick_length)->transpose;
	my $tick_size = $Tick_size / 2;
	
	my $top_bottom = pdl($clip_bottom, $clip_top)->transpose;
	my $left_right = pdl($clip_left, $clip_right)->transpose;
	
	my $em_height = $axis->{em_height};
	# working here - remove when pdl functions are 'atomic'
	my $lineWidth = $canvas->lineWidth;
	
	if ($axis->name eq 'x') {
		# Draw the minor tick marks:
		$canvas->pdl_lines($ticks_pixels, $top_bottom, $ticks_pixels, $top_bottom + $tick_size
				, lineWidths => 2);
		# Draw the major tick marks:
		$canvas->pdl_lines($Ticks_pixels, $top_bottom, $Ticks_pixels, $top_bottom + $Tick_size
				, lineWidths => 2);
		# Draw lines on the top/bottom:
		$canvas->pdl_lines($left_right->at(0,0), $top_bottom, $left_right->at(0,1), $top_bottom);
		
		# working here -remove when pdl operations are atomic
		$canvas->lineWidth($lineWidth);

		# Figure out the top of the axis labels:
		my $label_top = $clip_bottom - $axis->{em_height}/4;
		
		# Draw all the labels, storing the minimum and maximum extent used
		# for future calls to the draw function:
		my ($left_edge, $right_edge) = (1e20, 0);
		for (my $i = 0; $i < $Ticks->nelem; $i++) {
			my $x = $Ticks_pixels->at($i);
			my $string = sprintf("%1.8g", $Ticks->at($i));
			
			# Draw the label:
			$canvas->draw_text($string, $x-80, 0, $x+80, $label_top
				, dt::Top | dt::Center | dt::NoWordWrap
				| dt::UseExternalLeading);
			
			# Compute their widths:
			my $half_width = $canvas->get_text_width($string) / 2;
			$left_edge = $x - $half_width if $x - $half_width < $left_edge;
			$right_edge = $x + $half_width if $x + $half_width > $right_edge;
		}
		# Store these left and right edges:
		$axis->{edge_requirements}->[0]
			= ($clip_left > $left_edge + $em_height) ? $clip_left - $left_edge : $em_height;
		$axis->{edge_requirements}->[2]
			= ($right_edge > $clip_right + $em_height) ? $right_edge - $clip_right : $em_height;
		# Recompute the lowerEdge and edgeWidth.
		my @edges = $axis->owner->get_edge_requirements;
		$axis->{lowerEdge} = $edges[0];
		$axis->{edgeWidth} = $axis->owner->width - $edges[2] - $edges[0];
		
		# Draw the label, if it exists
		if ($axis->{label}) {
			$canvas->draw_text($axis->{label}, $clip_left, 0, $clip_right
				, 1.25 * $axis->{em_height}
				, dt::Center | dt::Top | dt::NewLineBreak | dt::NoWordWrap
					| dt::UseExternalLeading);
		}
	}
	else {
		# draw the minor tick marks:
		$canvas->pdl_lines($left_right, $ticks_pixels, $left_right + $tick_size, $ticks_pixels
				, lineWidths => 2);
		# draw the major tick marks:
		$canvas->pdl_lines($left_right, $Ticks_pixels, $left_right + $Tick_size, $Ticks_pixels
				, lineWidths => 2);
		# Draw lines on the left/right:
		$canvas->pdl_lines($left_right, $top_bottom->at(0,0), $left_right, $top_bottom->at(0,1));
		
		# working here -remove when pdl operations are atomic
		$canvas->lineWidth($lineWidth);
		
		# Figure out the right edge of the axis labels:
		my $label_right = $clip_left - $axis->{em_height}/4;
		
		# Draw all the labels, storing the minimum left edge used, so that
		# future calls to the draw function look reasonable:
		my $largest_width = 0;
		for (my $i = 0; $i < $Ticks->nelem; $i++) {
			my $y = $Ticks_pixels->at($i);
			my $string = sprintf("%1.8g", $Ticks->at($i));
			
			# Draw the label:
			$canvas->draw_text($string, 0, $y-80, $label_right, $y+80
			, dt::Right | dt::VCenter | dt::NoWordWrap
				| dt::UseExternalLeading);
			
			# Compute its left extent:
			my $points = $canvas->get_text_box($string);
			$largest_width = $points->[4] if $points->[4] > $largest_width;
		}
		$axis->{edge_requirements}->[0] = $largest_width + $em_height/2;
		$axis->{edge_requirements}->[0] += $em_height * 1.5
			if (defined $axis->{label} and $axis->{label} ne '');
		
		# Draw the label if it exists
		if ($axis->{label}) {
			# Determine the width of the text that we will draw:
			my $text_width = $canvas->get_text_width($axis->{label});
			
			# Rotate so we have vertical text. However, the rotation is
			# performed around the upper- (lower-?) left corner (regardless of the 
			# dt:: constants, as far as I can tell) and occurs after the
			# location of the text has been determined. It would be nice to
			# be able to specify the point about which the rotation should
			# take place.
			$canvas->font(direction => 90);
			# I'd like to say this:
			#$canvas->draw_text($axis->{label}, 0.25 * $em_height, $clip_bottom, 
			#	, 1.5 * $em_height, $clip_top
			#	, dt::VCenter | dt::Left | dt::NewLineBreak | dt::NoWordWrap
			#		| dt::UseExternalLeading);
			# But instead I have to say this:
			$canvas->draw_text($axis->{label}, 1.25 * $em_height
				, ($clip_top + $clip_bottom) / 2 - $text_width / 2,
				, 1.5 * $em_height, $clip_top,
				, dt::Bottom | dt::Left | dt::NewLineBreak | dt::NoWordWrap
					| dt::UseExternalLeading);
			$canvas->font(direction => 0);
		}
	}
}

1;

=head1 TODO

When drawing, I need to have the axes query the Scaling to see if any special
drawing needs to happen. I am thinking at the moment about broken axes.

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

Copyright (c) 2011 David Mertens. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
