use strict;
use warnings;

package PDL::Graphics::Prima;
our $VERSION = 0.04;

package Prima::Plot;
use PDL::Lite;
use Prima;
use Prima::ImageDialog;
use Prima::MsgBox;
use Prima::Utils;

use base 'Prima::Widget';

use Carp;
use PDL::NiceSlice;
use PDL::Drawing::Prima;

# I will need these graph-specific modules, too:
use PDL::Graphics::Prima::Axis;
use PDL::Graphics::Prima::DataSet;

=head1 NAME

PDL::Graphics::Prima - an interactive graph widget for PDL and Prima

=head1 SIMPLE SYNOPSIS

 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 # Generate some data - a sine curve
 my $x = sequence(100) / 20;
 my $y = sin($x);
 
 # Draw a point at each x/y pair:
 blob_plot($x, $y);
 
 # Draw a line connecting each x/y pair:
 line_plot($x, $y);
 
 # Draw a histogram:
 my ($bin_centers, $heights) = $y->hist;
 hist_plot($bin_centers, $heights);
 hist_plot($y->hist);  # equivalent
 
 
 # Generate some data - a wavy pattern
 my $image = sin(sequence(100)/10)
             + sin(sequence(100)/20)->transpose;
 
 # Generate a greyscale image:
 matrix_plot($image);
 
 # Set the    left, right,  bottom, top
 matrix_plot([0,    1],    [0,      2],  $image);
 
 
 # Use the more general plot for multiple datasets
 # and more plotting features:
 my $colors = pal::Rainbow()->apply($x);
 plot(
     -lines       => ds::Pair($x, $y),
     -color_blobs => ds::Pair($x, $y + 1,
                  colors => $colors,
                  plotType => pt::Blobs
     ),
     x => { label => 'Time' },
     y => { label => 'Sine' },
 );

=head1 WIDGET SYNOPSIS

 use PDL;
 use Prima qw(Application);
 use PDL::Graphics::Prima;
 
 my $t_data = sequence(6) / 0.5 + 1;
 my $y_data = exp($t_data);
 
 my $wDisplay = Prima::MainWindow->create(
     text  => 'Graph Test',
     size  => [300, 300],
 );
 
 $wDisplay->insert('Plot',
     -function => ds::Func(\&PDL::exp, color => cl::Blue),
     -data => ds::Pair($t_data, $y_data, color => cl::Red),
     pack => { fill => 'both', expand => 1},
 );
 
 run Prima;

=head1 IF YOU ARE NEW

If you are new to C<PDL::Graphics::Prima>, you should begin by reading the
documentation for L<PDL::Graphics::Prima::Simple>. This module provides a
simplified interface for quickly dashing off a few plots and offers stepping
stones to create more complex plots. Depending on your plotting needs, you may
not need anything more complicated than L<PDL::Graphics::Prima::Simple>. However,
C<PDL::Graphics::Prima> offers much greater flexibility and interactivity than
the options available in the Simple interface, so once you feel comfortable,
you should come back to this manual page and learn how to create and utilize
Plot widgets in conjunction with the Prima GUI toolkit.

=head1 DESCRIPTION

PDL::Graphics::Prima is a plotting interface for creating and exploring 2D data
visualizations. The core of this interace is a Plot widget that can be
incorporated into Prima applications. 

=cut

# Sets up a default profile for a graph widget
sub profile_default {
	my %def = %{$_[ 0]-> SUPER::profile_default};

	return {
		%def,
		# default properties go here
		title => '',
		titleSpace => 80,
		backColor => cl::White,
		# replot duration in milliseconds
		replotDuration => 30,
		# Blank profiles for the axes:
		x => {},
		y => {},
		selectable => 1,
		buffered => 1,
	};
}

# This initializes self's data from the profile:
sub init {
	my $self = shift;
	my %profile = $self->SUPER::init(@_);
	
	# Set the labels and title:
	$self->{title} = $profile{title};
	$self->{titleSpace} = $profile{titleSpace};
	
	# Create the x- and y-axis objects, overriding the owner and axis name
	# properties if they are set in the profile.
	$self->{x} = PDL::Graphics::Prima::Axis->create(
		  %{$profile{x}}
		, owner => $self
		, name => 'x'
		);
	$self->{y} = PDL::Graphics::Prima::Axis->create(
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
	
	# Create an empty dataset array and tie it to the DataSetHash class:
	my %datasets;
	tie %datasets, 'PDL::Graphics::Prima::DataSet::Collection', $self;
	$self->{dataSets} = \%datasets;
	
	# Turn off the axis autoscaling until after we've added the data
	$self->{x}->{initializing} = 1;
	$self->{y}->{initializing} = 1;
	
	# Add datasets. All of the datasets are validated when added as key/value
	# pairs to the tied hash:
	while (my ($key, $value) = each %profile) {
		next unless $key =~ /^-(.+)/;
		# $1 contains the name of the dataset; $value is the dataset itself:
		# working here - catch errors?
		$self->dataSets->{$1} = $value;
	}
	
	# Turn the axis autoscaling back on:
	$self->{x}->{initializing} = 0;
	$self->{y}->{initializing} = 0;
}

# This is key: *this* is what triggers autoscaling for the first time
# working here, consider setting sizeMin, sizeMax

sub on_size {
	my $self = shift;
	# Note: order here is important as x needs to access information about y
	# in its calculations
	$self->y->update_edges;
	$self->x->update_edges;
}

my $inf = -PDL->new(0)->log->at(0);


sub collect_collated_min_max_for;
sub pair_down_collation;

sub compute_min_max_for {
	my ($self, $axis_name) = @_;
	
	# Special handling for x-axis stuff:
	if ($axis_name eq 'x') {
		# First perform all of this on y so that the edge requirements are
		# correctly computed for x-calculations later.
		
		# Perform the collation:
		my ($trimmed_minima, $trimmed_maxima)
			= $self->collect_collated_min_max_for('y');
		return if not defined $trimmed_maxima;
		
		# Perform a round of minimazation:
		my ($min, $max)
			= $self->pair_down_collation('y', $trimmed_minima, $trimmed_maxima);
		
		# Update the axis min/max and redo the pair-down:
		$self->{'y'}->_min($min);
		$self->{'y'}->_max($max);
		$self->{'y'}->recalculate_edge_requirements($self);
	}
	
	# Perform the collation:
	my ($trimmed_minima, $trimmed_maxima)
			= $self->collect_collated_min_max_for($axis_name);
	return if not defined $trimmed_maxima;
	
	return $self->pair_down_collation($axis_name, $trimmed_minima, $trimmed_maxima);
}

sub get_pixel_extent_for;

sub collect_collated_min_max_for {
	my ($self, $axis_name) = @_;
	
	# Get plotting pixel extent, which I'll need to send to the dataSets in
	# order for them to compute their pyramids.
	my $pixel_extent = $self->get_pixel_extent_for($axis_name);
	return if $pixel_extent <= 0;
	
	my $datasets = $self->{dataSets};
	my (@min_collection, @max_collection);
	while (my ($key, $dataset) = each %$datasets) {
		next if $key eq 'widget';
		
		# Accumulate all the collated results
		my ($min, $max) = $dataset->compute_collated_min_max_for($axis_name, $pixel_extent);
		# The collated results should be one dimensional, so no reduction
		# necessary (as opposed to the DataSet code):
		push @min_collection, $min;
		push @max_collection, $max;
	}
	
	return if @min_collection == 0;
	
	# Merge all the data:
	my $collated_min = PDL::cat(@min_collection)->mv(-1,0)->minimum;
	my $collated_max = PDL::cat(@max_collection)->mv(-1,0)->maximum;
	
	# It could be the case that all the values are bad. In that case, insert
	# an entry at zero-pixels corresponding to the current view limits:
	$collated_min(0) .= $self->{$axis_name}->min if $collated_min->isbad->all;
	$collated_max(0) .= $self->{$axis_name}->max if $collated_max->isbad->all;
	
	# Iterativelye pair down the set until we've found the minmax. At this
	# point, we have two arrays with $pixel_extent elements each. Cat an
	# index and what will eventually be a computed value onto the original
	# lists so our slicing keeps track of the padding and original values.
	my $minima = $collated_min->cat($collated_min->sequence, $collated_min);
	my $maxima = $collated_max->cat($collated_max->sequence, $collated_max);

	# Get rid of all the bad values. We know that at least one good value will
	# remain due to the min/max insertion a few lines up.
	my $trimmed_minima = $minima->whereND($minima(:,0;-)->isgood);
	my $trimmed_maxima = $maxima->whereND($maxima(:,0;-)->isgood);
	
	return ($trimmed_minima, $trimmed_maxima);
}

sub pair_down_collation {
	my ($self, $axis_name, $trimmed_minima, $trimmed_maxima) = @_;
	
	my $min_mask = $trimmed_minima->trim_collated_min;
	my $max_mask = $trimmed_maxima->trim_collated_max;
	$trimmed_minima = $trimmed_minima->whereND($min_mask);
	$trimmed_maxima = $trimmed_maxima->whereND($max_mask);
	
	# Compute properly scaled extrema.
	# min_pix and max_pix are the plain pixel paddings needed by the lowest
	# element in the pyramid:
	my ($min_pix, $max_pix) = ($trimmed_minima->at(0,1), $trimmed_maxima->at(0,1));
	# virtual_pixel_extent is the room available to plot the data when the
	# real pixel extent is reduced by the requested pixel padding:
	my $pixel_extent = $self->get_pixel_extent_for($axis_name);
	my $virtual_pixel_extent = $pixel_extent - $min_pix - $max_pix;
	# min_data and max_data are the actual min and max values of the data
	# that are supposed to fit within the virtual pixel extent.
	my ($min_data, $max_data) = ($trimmed_minima->at(0,2), $trimmed_maxima->at(0,2));
	# min and max are the final minimal and maximal values that we use for
	# the axis so that all the data can be drawn within the plot.
	my $min = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
		, -$min_pix/$virtual_pixel_extent);
	my $max = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
		, 1 + $max_pix/$virtual_pixel_extent);
	
	# It is possible that all the x-data or all the y-data are identical. In
	# that case, this scheme would normally be degenerate and return nan,
	# which makes things croak. To solve this problem, we check if the
	# current min/max values are identical and return the scaling's response
	# in such a situation:
	return $self->{$axis_name}->scaling->min_max_for_degenerate($min)
		if $min == $max;
	
	# We will iterate until we stop removing rows, at which point the lowest
	# level of the pyramid is what we want.
	my $N_rows;
	do {
		$N_rows = $trimmed_minima->dim(0);
		
		# Assuming that the current min/max are roughly correct, compute the
		# updated min/max calculated values:
		$trimmed_minima(:,2)
			.= $self->{$axis_name}->pixels_to_reals(
				$self->{$axis_name}->reals_to_pixels($trimmed_minima(:,0), $min, $max)
					- $trimmed_minima(:,1), $min, $max);
		$trimmed_maxima(:,2)
			.= $self->{$axis_name}->pixels_to_reals(
				$self->{$axis_name}->reals_to_pixels($trimmed_maxima(:,0), $min, $max)
					+ $trimmed_maxima(:,1), $min, $max);
		
		# Trim again:
		$min_mask = $trimmed_minima->trim_collated_min;
		$max_mask = $trimmed_maxima->trim_collated_max;

		$trimmed_minima = $trimmed_minima->whereND($min_mask);
		$trimmed_maxima = $trimmed_maxima->whereND($max_mask);
		
		# Recompute the properly scaled extrema:
		($min_pix, $max_pix) = ($trimmed_minima->at(0,1), $trimmed_maxima->at(0,1));
		$virtual_pixel_extent = $pixel_extent - $min_pix - $max_pix;
		($min_data, $max_data) = ($trimmed_minima->at(0,0), $trimmed_maxima->at(0,0));
		$min = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
			, -$min_pix/$virtual_pixel_extent);
		$max = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
			, 1 + $max_pix/$virtual_pixel_extent);
	} while($N_rows != $trimmed_minima->dim(0));
	
	return ($min, $max);
}

sub get_pixel_extent_for {
	my ($self, $axis_name) = @_;
	
	my ($left, $bottom, $right, $top) = $self->get_edge_requirements;
	my $pixel_extent;
	if ($axis_name eq 'x') {
		$pixel_extent = $self->width - $left - $right;
	}
	else {
		$pixel_extent = $self->height - $top - $bottom;
	}
	return $pixel_extent;
}

# This should distinguish between shared space and exclusive space. For
# example, the title requests exclusive space (a height of titleSpace),
# which is added to that shared max shared space requested by both of the
# axes. The return values should be left, bottom, right, top
sub get_edge_requirements {
	my $self = shift;
	my @x_req = $self->x->get_edge_requirements;
	my @y_req = $self->y->get_edge_requirements;
	
	# Merge the two:
	my @requirement = (0, 0, 0, 0);
	my $i = 0;
	foreach my $req (@x_req, @y_req) {
		$requirement[$i] = $req if $requirement[$i] < $req;
		$i++;
		$i %= 4;
	}
	
	$requirement[3] += $self->{titleSpace} if $self->{title};
	
	return @requirement;
}

=head1 Properties

=head2 title, titleSpace

Sets or gets the string that contains the title and the space allocated for the
title at the top of the plot.

=cut

sub _title {
	$_[0]->{title} = $_[1];
}

sub title {
	return $_[0]->{title} unless $#_;
	$_[0]->_title($_[1]);
	$_[0]->notify('ChangeTitle');
}

sub _titleSpace {
	my ($self, $new_space) = @_;
	croak("titleSpace must be a positive integer")
		unless $new_space =~ /^\d+$/;
	
	# working here - tie in to sizeMin, sizeMax, and other things
	$self->{titleSpace} = $new_space;
}

sub titleSpace {
	return $_[0]->{titleSpace} unless $#_;
	$_[0]->_titleSpace($_[1]);
	$_[0]->notify('ChangeTitle');
}

=head2 dataSets

This either sets or returns the data sets. The data sets are held in a tied
anonymous hash that you directly manipulate. In order to add a new dataset,
you don't have to make a second call to dataSets; you can
simply modify the anonymous hash in place using standard Perl hash
manipulation functions. Since the hash is actually tied, datasets that you
add will be validated as you add them.

=cut

sub dataSets {
	# Return the (tied) hash ref if called as a getter:
	return $_[0]->{dataSets} unless $#_;
	# If called as a setter, copy all the values. This may seem inefficient,
	# but it automatically performs all the data validation for me:
	my ($self, %new_data) = @_;
	
	# Clear the dataset (except the widget key, whick never disappears):
	%{$self->{dataSets}} = ();
	
	# Add each dataset individually, to ensure the validation does what it's
	# supposed to do:
	while(my ($key, $dataset) = each %new_data) {
		$self->{dataSets}->{$key} = $dataset;
	}
	
	# Don't need to issue a notification because that already happened with
	# the above assignments.
	#$self->notify('ChangeData');
}

# For a change in title, recompute the autoscaling and replot.
sub on_changetitle {
	my $self = shift;
	$self->x->update_edges;
	$self->y->update_edges;
	$self->notify('Replot');
}

# for now, this is a replica of the above:
*on_changedata = \&on_changetitle;

# Sets up a timer in self that eventually calls the paint notification:
sub on_replot {
	my ($self) = @_;
	return if $self->{timer}->get_active;
	$self->{timer}->start;
}

=head1 Events

You can send notifications and hook callbacks for the following events:

=head2 ChangeTitle

Called when the title or titleSpace gets changed

=head2 Replot

Called when the widget needs to replot

=head2 ChangeData

Called when the dataSet container changes (not the datasets themselves, but
the whole container). 

=cut

#################
# Notifications #
#################
# Add a new notification_type for each of the notifications just defined.
{
	# Keep the notifications hash in its own lexically scoped block so that
	# other's can't mess with it.
	my %notifications = (
		%{Prima::Widget-> notification_types()},
		# working here - choose a better signal type?
		'Replot' => nt::Default,
		map { ("Change$_" => nt::Default) } qw(Title Data),
	);
	
	sub notification_types { return \%notifications }
}

sub on_paint {
	my ($self, $canvas) = @_;
	
	# Clear the canvas:
	$canvas = $self if not defined $canvas;
	$canvas->clear;
	
	# Get the clipping rectangle for the actual drawing space:
	my ($clip_left, $clip_bottom, $right_edge, $top_edge)
		= $self->get_edge_requirements;
	
	# The right and top edge values should be subtracted from the width and
	# height, respectively:
	my $clip_right = $canvas->width - $right_edge;
	my $clip_top = $canvas->height - $top_edge;
	
	# Clip the widget before we begin drawing
	$canvas->clipRect($clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# backup the drawing parameters:
	# working here - consider writing PDL::Drawing::Prima to back these up
	# for me automatically so I don't have to do it here
	# also, consider a better way than listing them here explicitly
	my @to_backup = qw(color backColor linePattern lineWidth lineJoin
			lineEnd rop rop2);
	my %backups = map {$_ => $canvas->$_} (@to_backup);
	
	# Draw the data, sorted by key name:
	foreach my $key (sort keys %{$self->{dataSets}}) {
		next if $key eq 'widget';
		my $dataset = $self->{dataSets}->{$key};
		$dataset->draw($canvas);
		
		# Restore the drawing parameters after each draw function:
		$canvas->set(%backups);
	}

	# Draw the zoom-rectangle, if there is one
	if (exists $self->{mouse_down_rel}->{mb::Right}) {
		my ($x, $y) = $self->pointerPos;
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_down_rel}->{mb::Right}};
		my $x_start_pixel = $self->x->relatives_to_pixels($x_start_rel);
		my $y_start_pixel = $self->y->relatives_to_pixels($y_start_rel);
		$canvas->rectangle($x_start_pixel, $y_start_pixel, $x, $y);
	}
	
	# Draw the axes
	$canvas->clipRect(0, 0, $self->size);
	$self->x->draw($canvas, $clip_left, $clip_bottom, $clip_right, $clip_top);
	$self->y->draw($canvas, $clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# Draw the title:
	if ($self->{titleSpace}) {
		my ($width, $height) = $canvas->size;
		# Set up the font characteristics:
		my $font_height = $canvas->font->height;
		$canvas->font->height($font_height * 1.5);
		my $style = $canvas->font->style;
		$canvas->font->style(fs::Bold);
		
		# Draw the title:
		$canvas->draw_text($self->{title}, 0, $height - $self->{titleSpace}
				, $width, $height
				, dt::Center | dt::VCenter | dt::NewLineBreak | dt::NoWordWrap
				| dt::UseExternalLeading);
		
		# Reset the font characteristics:
		$canvas->font->height($font_height);
		$canvas->font->style($style);
	}
}

# working here - the InputLine widget uses Prima::MouseScroller from IntUtils
# to handle things, and it appears to work. Maybe this can give me something
# that works better cross-platform.

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
	
	# On windows, the drag button is not properly reported. However, it is
	# there is never any issue with failure to report a mouse-down event (like
	# we get with X systems), so if I got here, it is easy enough to adapt.
	if ($^O =~ /MS/) {
		$drag_button = mb::Left if defined $self->{mouse_down_rel}->{mb::Left};
		$drag_button = mb::Right if defined $self->{mouse_down_rel}->{mb::Right};
	}
	
	if ($drag_button & mb::Left) {
		# A left mouse drag actually moves the graph around. Determine the
		# change in relative values, then change the min/max accordingly.
		
		# It sometimes happens that the mouse move event gets triggered
		# without a corresponding mouse down event, such as when I click
		# on the window from *another* application and move my mouse around.
		# If the mouse down coordinates are not known, store the current
		# ones and simply return.
		if (not defined $self->{mouse_down_rel}->{mb::Left}) {
			$self->{mouse_down_rel}->{mb::Left} = [$x_stop_rel, $y_stop_rel];
			return 1;
		}
		
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
	
	# Remove the previous button record for left and middle buttons:
	if ($up_button & mb::Left) {
		delete $self->{mouse_down_rel}->{mb::Left};
	}
	elsif ($up_button & mb::Middle) {
		delete $self->{mouse_down_rel}->{mb::Middle};
	}
	elsif ($up_button & mb::Right and defined $self->{mouse_down_rel}->{mb::Right}) {
		# Zoom in to the requested rectangle:
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_down_rel}->{mb::Right}};
		my $x_stop_rel = $self->x->pixels_to_relatives($x_stop_pixel);
		my $y_stop_rel = $self->y->pixels_to_relatives($y_stop_pixel);
		
		# Only rescale if there is a legitimate x- and y- box:
		if ($x_stop_rel != $x_start_rel and $y_stop_rel != $y_start_rel) {
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
		}
		# Call the popup menu if it 'looks' like a right-click:
		elsif ($x_stop_rel == $x_start_rel and $y_stop_rel == $y_start_rel) {
			$self->popup(Prima::Popup->new(
				items => [
					['~Copy' => sub {
						Prima::Timer->create(
							timeout => 250,
							onTick => sub {
								$_[0]->stop;
								$self->copy_to_clipboard;
							},
						)->start;
					}],
					['Save As ~Postscript...' => sub {
						$self->save_to_postscript;
					}],
					['~Save As...' => sub {
						Prima::Timer->create(
							timeout => 250,
							onTick => sub {
								$_[0]->stop;
								$self->save_to_file;
							},
						)->start;
					}],
					['~Autoscale' => sub {
							$self->x->minmax(lm::Auto, lm::Auto);
							$self->y->minmax(lm::Auto, lm::Auto);
					}],
				],
			));
		}
		# Remove the previous button record, so a zoom rectangle is not drawn:
		delete $self->{mouse_down_rel}->{mb::Right};
	}
}

sub get_image {
	my $self = shift;
	
# Not working:
#	# Build a prima image canvas and draw to it:
#	my $image = Prima::Image->create(
#		height => $self->height,
#		width => $self->width,
##		size => [$self->size],
#		backColor => $self->backColor,
#	) or die "Can't create an image!\n";
#	$self->on_paint($image);
#	return $image;

	return $::application->get_image($self->client_to_screen($self->origin), $self->size);
}

use Prima::PS::Drawable;
use Prima::FileDialog;

sub save_to_postscript {
	# Get the filename as an argument, or from the save-as dialog.
	my ($self, $filename) = @_;
	
	unless ($filename) {
		my $save_dialog = Prima::SaveDialog-> new(
			defaultExt => 'ps',
			filter => [
				['Postscript files' => '*.ps'],
				['All files' => '*'],
			],
		);
		# Return if they cancel out:
		return unless $save_dialog->execute;
		# Otherwise get the filename:
		$filename = $save_dialog->fileName;
	}
	unlink $filename if -f $filename;
	
	# Create the postscript canvas and plot to it:
	my $ps = Prima::PS::Drawable-> create( onSpool => sub {
			open my $fh, ">>", $filename;
			print $fh $_[1];
			close $fh;
		},
		pageSize => [$self->size],
		pageMargins => [0, 0, 0, 0],
		scale => [3, 3],
		font => {
			size => 8,
		},
	);
	
	$ps->begin_doc
		or do {
			my $message = "Error generating Postscript output: $@";
			if (defined $::application) {
				Prima::MsgBox::message($message, mb::Ok);
				carp $message;
			}
			else {
				croak($message);
			}
		};
	
	$self->on_paint($ps);
	$ps->end_doc;
}

# A routine to save the current plot to a rasterized file:
sub save_to_file {
	# Get the filename as an argument or from a save-as dialog.
	my ($self, $filename) = @_;
	
	# Get the image
	my $image = $self->get_image;
	
	# If they didn't specify a filename, run a dialog to get it:
	unless ($filename) {
		my $dlg = Prima::ImageSaveDialog-> create;
	
		$dlg->save($image);
		return;
	}
	
	# If they specified a filename, simply save it:
	$image-> save($filename)
		or do {
			my $message = "Error generating figure output: $@";
			if (defined $::application) {
				Prima::MsgBox::message($message, mb::Ok);
				carp $message;
			}
			else {
				croak($message);
			}
		};
}

sub copy_to_clipboard {
	my $self = shift;
	my $image = $self->get_image;
	
	my $clipboard = $::application->Clipboard;
	$clipboard->open;
	$clipboard->clear;
	$clipboard->image($image);
	$clipboard->close;
}

1;

=head1 RESPONSIBILITIES

The Plot object itself has to coordinate a number of sub-systems in order to get
a functioning plot. As these responsiblities are not always clear even to their
author, here is a list of what the plot is responsible for handling.

=over

=item knowing the plot title

Although the axes are responsible for knowing the axis labels, the plot itself
is responsible for knowing the plot title.

=item initiating drawing operations

The drawing of the axes, data, and plot title are all coordinated, ultimately,
by the plot object.

=item initiating autoscaling

Autoscaling for either of the axes is initiated by a call to the plot object's
C<compute_min_max_for>, which computes the minima and maxima for a given axis.
Most of the calculations necessary for this operation are performed by the
dataset and the underlying plotTypes, but they are coordinated and synthesized
by the plot object.

=item managing the dataset container

The plot does not manage the datasets directly, but it owns the dataset
container that is responsible for this.

=item handling user interaction

All user interaction (which at the moment is limited to mouse interaction) is
handled by the plot object. Drag and zoom events ultimately lead to changes in
the axes' minima and maxima (the axes are responsible for these pieces of
information), but these changes are initiated through the plot object.

=back

Many things are B<not> the responsiblity of the plot object but are instead
handled by other objects, usually held as objects within the plot object itself.

=over

=item tick and axis details

All axis and tick details, including minima and maxima, scaling, tick mark
locations, axis labels, and conversion from real x and y data values to pixel
offsets are handled by the axis objects.

=item managing datasets or plotTypes

The datasets are managed by the dataset collection hashref defined in
L<PDL::Graphics::Prima::DataSet>. Any access to the datasets (outside of 
declaring them directly in the constructor, which is a special-case) is managed
throug the dataSet collection. Any access to the specific plot types are 
themselves handled by the datasets themselves.

=back

=head1 TODO

Idea: set the ability to buffer replot notifications, so that you can fiddle
with datasets without triggering a replot. That should improve performance when
multiple datasets are dynamically added and removed. Then again, this is likely
to be a problem only with very large datasets.

This is not a perfect plotting library. Here are some of the particularly
annoying issues with it, which I hope to resolve:

adjustable right and bottom margins means mouse scroll-wheel action doesn't
work exactly as advertised

something's messed up with the x-ticks, and sometimes the left x-tick does not
line-up with the edge of the horizontal axis line.

linear tick scaling can switch the 'natural' major and minor ticks just by
moving, which makes it seem jittery. The function for determining the major and
minor ticks should be simplified and based only on the scale of interest.

singular properties (like C<color>, as opposed to C<colors>) do not Do What You
Mean. With PlotTypes, they often don't do anything at all. Although I could
resolve this problem in this library, I would rather address this in
L<PDL::Drawing::Prima>.

Shouldn't singular names => scalars, plural names => piddles be consistent
across the board? It's not, at least not with the plotTypes.

multiple axes. In the constructor, any property that starts with x would be an
x-axis (x1, x2, etc). You would have to specify an axes with a dataset, though
the default would be the first axis when sorted asciibetically. Axes would have
properties regarding if they are drawn on the top, the bottom, both, etc, and
whether their tick labels are drawn on the top, bottom, etc.

I am very proud of the automatic scaling. There are two drawbacks. (1) It's
complicated and not yet well documented. That needs fixing. (2) It could be even
more awesome. For example, it would be great to be able to specify a minimum 
pixel padding, as well as an extra pixel padding. This would simply effect how
collate_min_max_for_many works and should be a snap to implement.

Handle the titleSpace in a more intelligent way

=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 SEE ALSO

Both the L<Perl Data Language|PDL> and the L<Prima GUI Toolkit|Prima> are
amazing and this module would have no reason for existence without both of them.

This module serves as the motivation for L<PDL::Drawing::Prima>, and also would
be unable to function with any efficiency without it.

Other 2D plotting options include L<PDL::Graphics::PGPLOT>,
L<PDL::Graphics::PLplot>, L<PDL::Graphics::Gnuplot>, L<PDL::Graphics::Asymptote>,
and many others. Search CPAN for more.

For 3D plotting, see L<PDL::Graphics::TriD>.

Here is the full list of modules in this distribution:

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
