use strict;
use warnings;
$| = 1;

package Prima::Ex::Graph;
use PDL::Lite;
use Prima;

use base 'Prima::Widget';

use Carp qw(croak cluck);
use PDL::NiceSlice;
use PDL::Graphics::Prima;

# I will need these graph-specific modules, too:
use Prima::Ex::Graph::Limits;
use Prima::Ex::Graph::Axis;
use Prima::Ex::Graph::DataSet;

=head1 NAME

Prima::Graph::Simple - an interactive graph widget for Prima

=head1 SYNOPSIS

 working here

=cut

# Sets up a default profile for a graph widget
sub profile_default {
	my %def = %{$_[ 0]-> SUPER::profile_default};

	return {
		%def,
		# default properties go here
		xLabel => '',
		yLabel => '',
		title => '',
		backColor => cl::White,
		# replot duration in milliseconds
		replotDuration => 30,
		# Blank profiles for the axes:
		x => {},
		y => {},
	};
}

# This initializes self's data from the profile:
sub init {
	my $self = shift;
	my %profile = $self->SUPER::init(@_);
	foreach ( qw(xLabel yLabel title) ) {
		$self->{$_} = $profile{$_};
	}
	
	# Create the x- and y-axis objects, overriding the owner and axis name
	# properties if they are set in the profile.
	$self->{x} = Prima::Ex::Graph::Axis->create(
		  %{$profile{x}}
		, owner => $self
		, name => 'x'
		);
	$self->{y} = Prima::Ex::Graph::Axis->create(
		  %{$profile{y}}
		, owner => $self
		, name => 'y'
		);
	
	$self->{timer} = Prima::Timer->create(
		timeout => $profile{replotDuration},
		onTick => sub {
			$_[0]->stop;
			$self->notify('Paint');
		}
	);
	
	# Create an empty dataset array and tie it to the DataSetArray class:
	my @datasets;
	tie @datasets, 'Prima::Ex::Graph::DataSetArray', $self;
	$self->{dataSets} = \@datasets;
	
	# Add the datasets, if specified:
	$self->dataSets([$profile{dataSet}]) if (exists $profile{dataSet});
	$self->dataSets($profile{dataSets}) if (exists $profile{dataSets});
}

#sub x { return $_[0]->{x} }
#sub y { return $_[0]->{y} }

sub on_size {
	my ($self, undef, undef, $width, $height) = @_;
	$self->x->set_pixel_extent($width);
	$self->y->set_pixel_extent($height);
}

my $inf = -pdl(0)->log->at(0);

# Returns the computed extremum and padding from the datasets:
sub compute_data_extremum {
	my ($self, $extremum) = @_;
	# extremum should be one of the strings 'xmin', 'xmax', 'ymin', or 'ymax',
	# though it should be called using the constants defined above.
	
	# working here - explain this
	my ($to_return, $spaceship_wants) = ($inf, -1);
	($to_return, $spaceship_wants) = (-$inf, 1) if $extremum =~ /max$/;
	
	my ($most_extreme, $biggest_padding) = (undef, 0);
	# working here - I would like to do this:
#	foreach my $dataset ( @{$self->{dataSets}} ) {
	# but that's not working. Instead, I must resort to this:
	my $dataset;
	for(my $i = 0; exists $self->{dataSets}->[$i]; $i++) {
		$dataset = $self->{dataSets}->[$i];
		
		# Get the dataset's extremum and padding
		my ($extremum, $padding) = $dataset->extremum($extremum, $self, $spaceship_wants);
		next if not defined $extremum;
		
		# Keep track of the largest padding:
		$biggest_padding = $padding if $biggest_padding < $padding;
		# Only save the value if it is the most extreme:
		$most_extreme = $extremum
			if not defined $most_extreme
				or ($extremum <=> $most_extreme) == $spaceship_wants;
	}
	
	return ($most_extreme, $biggest_padding);
}

=head1 Properties

=head2 xLabel, yLabel, title

Sets or gets the various strings for the axis labeling and the title.

=cut

sub xLabel {
	return $_[0]->{xLabel} unless $#_;
	$_[0]->{xLabel} = $_[1];
	$_[0]->notify('ChangeXLabel');
}
sub yLabel {
	return $_[0]->{yLabel} unless $#_;
	$_[0]->{yLabel} = $_[1];
	$_[0]->notify('ChangeYLabel');
}
sub title {
	return $_[0]->{title} unless $#_;
	$_[0]->{title} = $_[1];
	$_[0]->notify('ChangeTitle');
}

=head2 dataSets

This either sets or returns the data sets. The data sets are held in a tied
anonymous array that you directly manipulate. In order to add a new dataset,
you don't have to make a second call to dataSets; you can
simply modify the anonymous array in place using standard Perl array
manipulation functions. Since the array is actually tied, datasets that you
add will be validated as you add them.

=cut

sub dataSets {
	# Return the (tied) array ref if called as a getter:
	return $_[0]->{dataSets} unless $#_;
	# If called as a setter, copy all the values. This may seem inefficient,
	# but it automatically performs all the data validation for me:
	my ($self, $new_data) = @_;
	
	foreach(@$new_data) {
		push @{$self->{dataSets}}, $_;
	}
	
	# Finish by issuing a notification:
	$_[0]->notify('ChangeData');
}

=head2 add_data

This is a convenience function to save on typing, since the most common
modification to the dataset array is likely to be the addition of new data.

 # Add a data set to the graph widget:
 $graph_widget->add_data($x, $y, \%opts);

This only adds one dataset at a time. If you want to add many, just use the tied
array interface:

 my $arr_ref = $graph_widget->dataSets;
 push @$arr_ref, [$x, $y, \%opts], [\&PDL::sin];

=cut

sub add_data {
	my $self = shift;
	push @{$self->dataSets}, \@_;
}

# For any of these events, repaint:
sub _repaint {$_[0]->notify('Replot')}
*on_changetitle = \&_repaint;
*on_changexlabel = \&_repaint;
*on_changeylabel = \&_repaint;

# Sets up a timer in self that eventually calls the paint notification:
sub on_replot {
	my ($self) = @_;
	return if $self->{timer}->get_active;
	$self->{timer}->start;
}

=head1 Events

You can send notifications and tie callbacks for the following events:

=head2 ChangeTitle

=head2 ChangeXLabel

=head2 ChangeYLabel

=cut

#################
# Notifications #
#################
# Add a new notification_type for each of the notifications just defined.
{
	# Keep the notifications hash in its own lexically scoped block so that
	# other's can't mess with it (at least, not without using PadWalker or some
	# such).
	my %notifications = (
		%{Prima::Widget-> notification_types()},
		# working here - choose a better signal type
		'Replot' => nt::Default,
		map { ("Change$_" => nt::Default) } qw(Title XLabel YLabel Data),
	);
	
	sub notification_types { return \%notifications }
}

sub on_paint {
	my ($self) = @_;
	
	# Clear the canvas:
	$self->clear;
	
	# Get the clipping rectangle from the axes:
	my ($clip_left, $clip_right) = $self->x->viewMinMax;
	$clip_left *= $self->width;
	$clip_right *= $self->width;
	my ($clip_bottom, $clip_top) = $self->y->viewMinMax;
	$clip_bottom *= $self->height;
	$clip_top *= $self->height;
	$self->clipRect($clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# backup the drawing parameters:
	my @to_backup = qw(color backColor linePattern lineWidth lineJoin
			lineEnd rop rop2);
	my %backups = map {$_ => $self->$_} (@to_backup);
	
	# Draw the data:
	for (my $i = 0; exists $self->{dataSets}->[$i]; $i++) {
		my $dataset = $self->{dataSets}->[$i];
		$dataset->draw($self);
		
		# Restore the drawing parameters after each draw function:
		$self->set(%backups);
	}

	# Draw the zoom-rectangle, if there is one
	if (exists $self->{mouse_down_rel}->{mb::Right}) {
		my ($x, $y) = $self->pointerPos;
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_down_rel}->{mb::Right}};
		my $x_start_pixel = $self->x->relatives_to_pixels($x_start_rel);
		my $y_start_pixel = $self->y->relatives_to_pixels($y_start_rel);
		$self->rectangle($x_start_pixel, $y_start_pixel, $x, $y);
	}
	
	# Draw the axes
	$self->clipRect(0, 0, $self->size);
	$self->x->draw($self);
	$self->y->draw($self);
	
	# Draw the axis labels and title:
	# working here - I need to clean this up a bit
	my ($width, $height) = $self->size;
	if ($self->{xlabel}) {
		$self->draw_text($self->{xlabel}
			, 0, 0, $width, $height * $self->y->viewMin / 2
			, dt::Center | dt::Top
			);
	}
	if ($self->{ylabel}) {
		$self->font(direction => 90);
		$self->draw_text($self->{ylabel}
			, 0, 0, $width * $self->x->viewMin, $height
			, dt::VCenter | dt::Right
		);
		$self->font(direction => 0);
	}
	warn("No title, yet");
}

# For mousewheel events, we zoom in or out. However, if they're over the axes,
# only zoom in or out for that axis.
sub on_mousewheel {
	my ($self, $mods, $x, $y, $dir) = @_;
	my ($width, $height) = $self->size;
	
	# Get the relative x and y positions. When the mouse is over the plot itself
	# this is a number between 0 and 1 for both x and y. When the house is, for
	# example, over the x-axis tick labels, the y value will be less than 0.
	my $rel_x = $self->x->pixels_to_relatives($x);
	my $rel_y = $self->y->pixels_to_relatives($y);
	
	# if the mouse is over the data or the x-axis, zoom in the x-direction,
	# preserving the position of the mouse's x-value:
	if ($rel_x > 0 and $rel_x < 1) {
		# Dir > 0 means zooming in:
		my ($rel_min, $rel_max) = (0, 1);
		if ($dir > 0) {
			$rel_min += $rel_x/5;
			$rel_max -= (1 - $rel_x) / 5;
		}
		else {
			$rel_min -= $rel_x/5;
			$rel_max += (1 - $rel_x) / 5;
		}
		
		# Compute the new min/max values from the axis scaling:
		my $real_min = $self->x->relatives_to_reals($rel_min);
		my $real_max = $self->x->relatives_to_reals($rel_max);
		$self->x->minmax($real_min, $real_max);
	}
	# if the mouse is over the data or the y-axis, zoom in the y-direction:
	if ($rel_y > 0 and $rel_y < 1) {
		# Dir > 0 means zooming in:
		my ($rel_min, $rel_max) = (0, 1);
		if ($dir > 0) {
			$rel_min += $rel_y/5;
			$rel_max -= (1 - $rel_y) / 5;
		}
		else {
			$rel_min -= $rel_y/5;
			$rel_max += (1 - $rel_y) / 5;
		}
		
		# Compute the new min/max values from the axis scaling:
		my $real_min = $self->y->relatives_to_reals($rel_min);
		my $real_max = $self->y->relatives_to_reals($rel_max);
		$self->y->minmax($real_min, $real_max);
	}
	
	$self->notify('Replot');
}

sub get_min_max_for {
	my ($first, $second) = @_;
	return ($first, $second) if $first < $second;
	return ($second, $first);
}

sub on_mousedown {
	my ($self, $down_button, undef, $x, $y) = @_;
	# Store the relative click locations:
	$x = $self->x->pixels_to_relatives($x);
	$y = $self->y->pixels_to_relatives($y);
	foreach (mb::Left, mb::Right, mb::Middle) {
		$self->{mouse_down_rel}->{$_} = [$x, $y] if $down_button & $_;
	}
	$self->{mouse_move_rel} = [$x, $y];
}

sub on_mousemove {
	my ($self, $drag_button, $x_stop_pixel, $y_stop_pixel) = @_;
	
	# Compute the relative and real final mouse locations
	my $x_stop_rel = $self->x->pixels_to_relatives($x_stop_pixel);
	my $y_stop_rel = $self->y->pixels_to_relatives($y_stop_pixel);
	
	if ($drag_button & mb::Left) {
		# A left mouse drag actually moves the graph around. Determine the
		# change in relative values, then change the min/max accordingly.
		
		# working here - per-button mouse click tracking? I ask because sometimes
		# I accidentially click the left mouse button when I'm selecting a zoom
		# rectangle and it messed things up.
		
		# The behavior depends upon where they initially clicked and where the
		# mouse was last, so retrieve both of those values:
		my ($x_down_rel, $y_down_rel) = @{$self->{mouse_down_rel}->{mb::Left}};
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_move_rel}};
		
		# If the initial click was within the x-boundaries, then the x-values
		# should be adjusted:
		if ($x_down_rel > 0 and $x_down_rel < 1) {
			# Determine the relative change and the adjusted min/max:
			my $dx = $x_stop_rel - $x_start_rel;
			my $new_min = $self->x->relatives_to_reals(-$dx);
			my $new_max = $self->x->relatives_to_reals(1 - $dx);
			$self->x->minmax($new_min, $new_max);
		}
		# If the initial click was within the y-boundaries, then the y-values
		# should be adjusted:
		if ($y_down_rel > 0 and $y_down_rel < 1) {
			# Determine the relative change and the adjusted min/max:
			my $dy = $y_stop_rel - $y_start_rel;
			my $new_min = $self->y->relatives_to_reals(-$dy);
			my $new_max = $self->y->relatives_to_reals(1 - $dy);
			$self->y->minmax($new_min, $new_max);
		}
	}
	if ($drag_button & mb::Right) {
		$self->notify('Replot');
	}

	# Store the intermediate locations:
	$self->{mouse_move_rel} = [$x_stop_rel, $y_stop_rel];
}

sub on_mouseup {
	my ($self, $up_button, $up_mods, $x_stop_pixel, $y_stop_pixel) = @_;
	
	if ($up_button & mb::Left) {
		delete $self->{mouse_down_rel}->{mb::Left};
	}
	if ($up_button & mb::Right) {
		# Zoom in to the requested rectangle:
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_down_rel}->{mb::Right}};
		my $x_stop_rel = $self->x->pixels_to_relatives($x_stop_pixel);
		my $y_stop_rel = $self->y->pixels_to_relatives($y_stop_pixel);
		
		# Reset the x min/max
		my ($min_rel, $max_rel) = get_min_max_for($x_start_rel, $x_stop_rel);
		# Compute the new min/max values from the axis scaling:
		my $min_real = $self->x->relatives_to_reals($min_rel);
		my $max_real = $self->x->relatives_to_reals($max_rel);
		# Set the new min/max values:
		$self->x->minmax($min_real, $max_real);

		# Reset the y min/max
		($min_rel, $max_rel) = get_min_max_for($y_start_rel, $y_stop_rel);
		# Compute the new min/max values from the axis scaling:
		$min_real = $self->y->relatives_to_reals($min_rel);
		$max_real = $self->y->relatives_to_reals($max_rel);
		# Set the new min/max values:
		$self->y->minmax($min_real, $max_real);
		
		# Remove the previous button record, so a zoom rectangle is not drawn:
		delete $self->{mouse_down_rel}->{mb::Right};
	}
	if ($up_button & mb::Middle) {
		# Remove the previous button record
		delete $self->{mouse_down_rel}->{mb::Middle};
	}
}

1;

__END__

	$self->onMouseClick( sub {
		my (undef, $button, undef, $mouse_x, $mouse_y, $double_click) = @_;
		if ($double_click and $button == mb::Left) {
			my ($x_max_pixel, $y_max_pixel) = $self->size;
			# Is this an auto-scale double-click?
			if ($mouse_x < $x_max_pixel / 10) {
				($ymin, $ymax) = $y->where(($xmin < $x) & ($x < $xmax))->minmax;
				$self->notify("Paint");
				$self->notify("PostMessage", 'new-range')
			}
			elsif ($mouse_y < $y_max_pixel / 10) {
				($xmin, $xmax) = $x->minmax;
				$self->notify("Paint");
				$self->notify("PostMessage", 'new-range')
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
				$self->notify("PostMessage", 'new-range')
			}
