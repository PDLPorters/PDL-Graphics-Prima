use strict;
use warnings;

############################################################################
                       package PDL::Graphics::Prima;
############################################################################
our $VERSION = 0.13;

# Add automatic support for PDL terminal interactivity
use PDL::Graphics::Prima::ReadLine;
sub import {
	my $class = shift;
	
	# Set up the interactivity, if possible with this terminal
	PDL::Graphics::Prima::ReadLine->setup($PERLDL::TERM)
		if PDL::Graphics::Prima::ReadLine->is_happy_with($PERLDL::TERM);
}

############################################################################
                           package Prima::Plot;
############################################################################

# Prima
use Prima;
use Prima::ImageDialog;
use Prima::MsgBox;
use Prima::Utils;

use base 'Prima::Widget';

# Error reporting
use Carp;

# PDL
use PDL::Lite;
use PDL::NiceSlice;
use PDL::Drawing::Prima;

# library-specific modules whose functionality I need
use PDL::Graphics::Prima::Axis;
use PDL::Graphics::Prima::DataSet;

# Next: use block-comments to describe the purpose of each method.

######################################
# Name       : 
# Arguments  : 
# Invocation : 
# Purpose    : 
# Returns    : 
# Throws     : 
# Comments   : 

######################################
# Name       : profile_default
# Arguments  : I'm not entirely sure, but it's either the class 
# Invocation : 
# Purpose    : Sets up a default profile for a graph widget
# Returns    : 
# Throws     : never
# Comments   : 
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
	for ('x', 'y') {
		if (eval{$profile{$_}->isa('PDL::Graphics::Prima::Axis')}) {
			$self->{$_} = $profile{$_};
			$self->{$_}->owner($self);
			$self->{$_}->name($_);
		}
		elsif (ref ($profile{$_}) eq 'HASH') {
			$self->{$_} = PDL::Graphics::Prima::Axis->create(
				%{$profile{$_}}
				, owner => $self
				, name => $_
			);
		}
		else {
			croak("Unable to create $_-axis from $profile{$_}");
		}
	}
	
	$self->{timer} = Prima::Timer->create(
		timeout => $profile{replotDuration},
		onTick => sub {
			$_[0]->stop;
			$self->repaint;
		}
	);
	
	$self->{log} = 'test';
	
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
	my (undef, $y_max_is_auto) = $self->y->max;
	if ($axis_name eq 'x' and $y_max_is_auto) {
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
		(undef, my $min_auto) = $self->y->min;
		(undef, my $max_auto) = $self->y->max;
		$self->y->_min($min);
		$self->y->_max($max);
		$self->y->recalculate_edge_requirements($self);
		$self->y->_min(lm::Auto) if $min_auto;
		$self->y->_max(lm::Auto) if $max_auto;
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

# Given a collated set of mininam and maxima (from collect_collated_min_max_for)
# this function determines that minimum and maximum necessary for viewing *all*
# the data.

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
	# working here - come up with something better than just croaking.
	die "Internal error: virtual pixel extent is non-positive"
		if $virtual_pixel_extent <= 0;
	# min_data and max_data are the actual min and max values of the data
	# that are supposed to fit within the virtual pixel extent.
	my ($min_data, $max_data)
		= ($trimmed_minima(:,(2))->min, $trimmed_maxima(:,(2))->max);
	
	# It is possible that all the x-data or all the y-data are identical.
	# Check if the current min/max values are identical and return the
	# scaling's response in such a situation:
	return $self->{$axis_name}->scaling->min_max_for_degenerate($min_data)
		if $min_data == $max_data;
	
	# min and max are the final minimal and maximal values that we use for
	# the axis so that all the data can be drawn within the plot.
	my $min = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
		, -$min_pix/$virtual_pixel_extent);
	my $max = $self->{$axis_name}->scaling->inv_transform($min_data, $max_data
		, 1 + $max_pix/$virtual_pixel_extent);
	# working here - come up with something better than just croaking.
	die "Internal error: min ($min) is greater than max ($max)" if $min > $max;
	
	# We will iterate until we stop removing rows, at which point the lowest
	# level of the pyramid is what we want.
	my $N_rows;
	do {
		$N_rows = $trimmed_minima->dim(0);
		
		# Assuming that the current min/max are roughly correct, compute the
		# updated min/max calculated values:
		$trimmed_minima(:,2)
			.= $self->{$axis_name}->pixels_to_reals(
				$self->{$axis_name}->reals_to_pixels($trimmed_minima(:,0), 1, $min, $max)
					- $trimmed_minima(:,1), 1, $min, $max);
		$trimmed_maxima(:,2)
			.= $self->{$axis_name}->pixels_to_reals(
				$self->{$axis_name}->reals_to_pixels($trimmed_maxima(:,0), 1, $min, $max)
					+ $trimmed_maxima(:,1), 1, $min, $max);
		
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
	
	$requirement[3] += $self->{titleSpace}
		if defined $self->{title} and $self->{title} ne '';
	
	return @requirement;
}

#############################################
# Name   : 

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

sub get_image {
	my $self = shift;
	
	# Build a prima image canvas and draw to it:
	my $image = Prima::Image->create(
		height => $self->height,
		width => $self->width,
		backColor => $self->backColor,
	) or die "Can't create an image!\n";
	$image->begin_paint or die "Can't draw on image";
	$self->on_paint($image);
	$image->end_paint;
	return $image;
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
	);
	$ps->resolution($self->resolution);
	$ps->font(size => 16);
	
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

# For a change in title, recompute the autoscaling and issue an immediate
# repaint. Replotting is not appropriate here as replotting issues a timer
# event that may not get triggered if the event loop isn't running (i.e.
# we're in the PDL shell without ReadLine integration).
sub on_changetitle {
	my $self = shift;
	$self->x->update_edges;
	$self->y->update_edges;
	$self->notify('Paint');
	# Clear the event queue so this hits immediately in the PDL shell
	$::application->yield if defined $::application;
}

# Sets up a timer in self that eventually calls the paint notification:
sub on_replot {
	my ($self) = @_;
	return if $self->{timer}->get_active;
	$self->{timer}->start;
}

# for now, this is a replica of the above:
*on_changedata = \&on_changetitle;

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
	
	my $ratio = $canvas->height / $self->height;
	
	# The right and top edge values should be subtracted from the width and
	# height, respectively:
	my $clip_right = $canvas->width - $ratio * $right_edge;
	my $clip_top = $canvas->height - $ratio * $top_edge;
	
	# Correct the left and bottom clipping for the canvas ratio
	$clip_left *= $ratio;
	$clip_bottom *= $ratio;
	
	# Clip the widget before we begin drawing
	$canvas->clipRect($clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# Draw the data, sorted by key name:
	foreach my $key (sort keys %{$self->{dataSets}}) {
		next if $key eq 'widget';
		$self->{dataSets}->{$key}->draw($canvas, $ratio);
	}

	# Draw the zoom-rectangle, if there is one
	if (exists $self->{mouse_down_rel}->{mb::Right}) {
		my ($x, $y) = $self->pointerPos;
		my ($x_start_rel, $y_start_rel) = @{$self->{mouse_down_rel}->{mb::Right}};
		my $x_start_pixel = $self->x->relatives_to_pixels($x_start_rel, $ratio);
		my $y_start_pixel = $self->y->relatives_to_pixels($y_start_rel, $ratio);
		$canvas->rectangle($x_start_pixel, $y_start_pixel, $x, $y);
	}
	
	# Draw the axes
	$canvas->clipRect(0, 0, $self->size);
	$self->x->draw($canvas, $clip_left, $clip_bottom, $clip_right, $clip_top, $ratio);
	$self->y->draw($canvas, $clip_left, $clip_bottom, $clip_right, $clip_top, $ratio);
	
	# Draw the title:
	if ($self->{titleSpace}) {
		my ($width, $height) = $canvas->size;
		# Set up the font characteristics:
		my $font_height = $canvas->font->height;
		$canvas->font->height($font_height * 1.5);
		my $style = $canvas->font->style;
		$canvas->font->style(fs::Bold);
		
		# Draw the title:
		$canvas->draw_text($self->{title}, 0, $height - $self->{titleSpace} * $ratio
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
			
			# Call the non-notifying version. The notifying version causes an
			# immediate redraw that causes the plot to accelerate away.
			$self->x->_min($new_min);
			$self->x->_max($new_max);
		}
		# If the initial click was within the y-boundaries, then the y-values
		# should be adjusted:
		if ($y_down_rel > 0 and $y_down_rel < 1) {
			# Determine the relative change and the adjusted min/max:
			my $dy = $y_stop_rel - $y_start_rel;
			my $new_min = $self->y->relatives_to_reals(-$dy);
			my $new_max = $self->y->relatives_to_reals(1 - $dy);
			
			# Call the non-notifying version. The notifying version causes an
			# immediate redraw that causes the plot to accelerate away.
			$self->y->_min($new_min);
			$self->y->_max($new_max);
		}
	}
	
	# Repaint if they're dragging the mouse
	$self->notify('Replot') if $drag_button;

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

1;

__END__

=head1 NAME

PDL::Graphics::Prima - an interactive plotting widget and library for PDL and Prima

=head1 SIMPLE SYNOPSIS

 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 
 # --( Super simple line and symbol plots )--
 
 # Generate some data - a sine curve
 my $x = sequence(100) / 20;
 my $y = sin($x);
 
 # Draw x/y pairs. Default x-value are sequential:
 line_plot($y);        line_plot($x, $y);
 circle_plot($y);      circle_plot($x, $y);
 triangle_plot($y);    triangle_plot($x, $y);
 square_plot($y);      square_plot($x, $y);
 diamond_plot($y);     diamond_plot($x, $y);
 X_plot($y);           X_plot($x, $y);
 cross_plot($y);       cross_plot($x, $y);
 asterisk_plot($y);    asterisk_plot($x, $y);
 
 # Sketch the sine function for x initially from 0 to 10:
 func_plot(0 => 10, \&PDL::sin);
 
 
 # --( Super simple histogram )--
 
 # PDL hist method returns x/y data
 hist_plot($y->hist);
 my ($bin_centers, $heights) = $y->hist;
 hist_plot($bin_centers, $heights);
 # Even simpler, if of limited use:
 hist_plot($heights);
 
 
 # --( Super simple matrix plots )--
 
 # Generate some data - a wavy pattern
 my $image = sin(sequence(100)/10)
             + sin(sequence(100)/20)->transpose;
 
 # Generate a grayscale image:
 matrix_plot($image);  # smallest is white
 imag_plot($image);    # smallest is black
 
 # Set the x and y coordinates for the image boundaries
 #            left, right,  bottom, top
 matrix_plot([ 0,     1  ], [ 0,     2 ],  $image);
 imag_plot(  [ 0,     1  ], [ 0,     2 ],  $image);
 
 
 # --( More complex plots )--
 
 # Use the more general 'plot' function for
 # multiple DataSets and more plotting features:
 my $colors = pal::Rainbow()->apply($x);
 plot(
     -lines         => ds::Pair($x, $y
         , plotType => ppair::Lines
     ),
     -color_squares => ds::Pair($x, $y + 1
         , colors   => $colors,
         , plotType => ppair::Squares(filled => 1)
     ),
     
     x => { label   => 'Time' },
     y => { label   => 'Sine' },
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

If you are new to PDL::Graphics::Prima, you should begin by reading the
documentation for L<PDL::Graphics::Prima::Simple|PDL::Graphics::Prima::Simple/>.
This module provides a simplified interface for quickly dashing off a few
plots and offers stepping stones to create more complex plots. Depending on
your plotting needs, you may not need anything more complicated than
L<PDL::Graphics::Prima::Simple|PDL::Graphics::Prima::Simple/>. However,
PDL::Graphics::Prima offers much greater flexibility and interactivity than
the options available in the Simple interface, so once you feel comfortable,
you should come back to this manual page and learn how to create and utilize
Plot widgets in conjunction with the L<Prima GUI toolkit|Prima/>.

=head1 DESCRIPTION

PDL::Graphics::Prima is a plotting library for 2D data visualization. The
core of this library is a Plot widget that can be incorporated into Prima
applications. Although the library is capable of producing nice looking
static figures, its killer feature is the GUI environment in which it
belongs. L<Prima> provides an array of useful interactive widgets and a
simple but powerful event-based programming model. PDL::Graphics::Prima
provides a sophisticated plotting library within this GUI framework, letting
you focus on what you want to visualize rather than the details of how you
would draw it. These tools allow you to build interactive data
visualization and analysis applications with sophisticated plotting and
intuitive user interaction in only a few hundred lines of code.

Like any other widget, a Plot widget can be constructed using the parent
widget's L<insert method|Prima::Widget/insert>. PDL::Graphics::Prima
actually defines the bulk of its functionality in the Prima::Plot package,
so that you can simply say:

 $parent->insert(Plot =>
     place => {
         x => 0, relwidth => 0.5, anchor => 'sw',
         y => 0, relheight => 0.5,
     },
     -data => ds::Pair($t_data, $y_data, color => cl::Red),
     ... etc ...
 );

Prima::Plot (i.e. PDL::Graphics::Prima) is a descendant of the
L<Prima::Widget|Prima::Widget/> class, so everything that you can do with
widgets you can do with plots, including specifying L<event|Prima::Widget/Events>
L<callbacks|Prima::Object/Events> such as L<mouse|Prima::Widget/Mouse> and
L<keyboard|Prima::Widget/Keyboard> interaction. You can specify the means
for placing the plot within a larger parent widget using
L<basic geometry management|Prima::Widget/Geometry>, or the Tk-like
L<pack|Prima::Widget::pack/> or L<place|Prima::Widget::place/> specifiers.
In fact, Prima allows any widget to serve as the container for other widgets,
so you can insert other widgets (i.e. other plots) into a plot. This serves
as a simple mechanism for creating sub-plots, though beware: sub-plots are
not yet correctly rendered in raster or postscript file output.

If you want to add new content to a plot or remove content from a plot, you
do this by manipulating the L<dataSet collection|/dataSets>. Axis
L<minima|PDL::Graphics::Prima::Axis/"min, max">,
L<maxima|PDL::Graphics::Prima::Axis/"min, max">,
L<scaling|PDL::Graphics::Prima::Axis/scaling>, and
L<labels|PDL::Graphics::Prima::Axis/label> are handled by
L<axis objects|PDL::Graphics::Prima::Axis/> which you obtain through the
L<x and y axis accessors|/"x, y">. You set and manipulate the
title via the L<title accessor method|/title>.

From the standpoint of basic plot object structure, that's about it!

=head1 Properties

PDL::Graphics::Prima has a number of properties that you can specify in the
constructor and later change through accessor methods.

=head2 title

Sets or gets the string with the figure's title text. To remove an already
set title, specify an empty string or the undefined value. Changing this
issues a L<ChangeTitle> event.

=head2 titleSpace

Sets or gets the number of pixels that would be allocated for the title, if
the title is set. Changing this issues a L<ChangeTitle> event, even if the
title text is not presently being displayed.

Note: This is a fairly lame mechanism for specifying the space needed for the
title. Expect this to change, for the better, in the future. Please drop me
a note if you use this and need any such changes to be backwards compatible.

=head2 x, y

Obtains the object that controls the settings for the x- or
y-L<axis|PDL::Graphics::Prima::Axis>. For example:

 # Set the x-min to -10 and the y-max to auto-scaling
 $plot->x->min(-10);
 $plot->y->max(lm::Auto);

Actually, these accessors are not hard-coded into the plot library. Rather,
these are the default L<name|Prima::Object>s of the axes. Any object of
type Prima::Component (which is any object in the Prima object heierarchy)
that has a name can be accessed from the parent by using the component's
name as a method on the parent. That is, you can change the name of the
axis and use the new name:

 # Rename the x-axis; be sure it starts with "x", though
 $plot->x->name('xfoo');
 # Change the x-axis' minimum value
 $plot->xfoo->min(-10);
 # This croaks:
 $plot->x->max(20);

This is a L<feature of Prima|Prima::Object/bring>. Eventually, when multiple
x- and y-axes are allowed, this will allow us to transparently access them by
name just like we access the single x- and y-axes by name at the moment.

=head2 dataSets

This is the means by which you add new content to your plot (apart from
placing sub-figures in there, of course). This either sets or returns the
L<collection|PDL::Graphics::Prima::DataSet/DataSet::Collection> of
L<DataSet|PDL::Graphics::Prima::DataSet>s. The
L<DataSet|PDL::Graphics::Prima::DataSet>s are held in a tied
anonymous hash that you directly manipulate. In order to add a new
L<DataSet|PDL::Graphics::Prima::DataSet>, you can simply modify the anonymous
hash in place using standard Perl hash manipulation functions and techniques.
For example:

 # Add a new DataSet
 $plot->dataSets->{new_data} = ds::Pair(
     $x, $y, plotType => ppair::Squares
 );
 
 # Remove a DataSet
 delete $plot->dataSets->{model};
 
 # Clear the DataSets
 %{$plot->dataSets} = ();

Since the hash is actually tied, L<DataSet|PDL::Graphics::Prima::DataSet>s
that you add will be validated as you add them.


=head1 METHODS

PDL::Graphics::Prima provides a number of methods. Most of these focuse on
generating images of the plot.

=head2 get_image

Returns a L<Prima::Image> of the plot with same dimensions as the plot widget.

=head2 save_to_postscript

Saves the plot with current axis limits to a postscript figure. This method
takes an optional filename argument. If no filename is specified, it pops-up
a dialog box to ask the user where and under what name they want to save the
postscript figure.

This functionality will likely be merged into save_to_file, though this
method will remain for backwards compatibility.

=head2 save_to_file

Saves the plot to a raster image file. This method
takes an optional filename argument, deducing the format (and applicable
codec) from the filename. If no filename is specified, it creates a dialog
box asking the user where and under what name they want to save the file.

=head2 copy_to_clipboard

Copies the plot with current axis limits as a bitmap image to the clipboard.
The resulting clipboard entry is suitable for pasting into applications that
know how to handle bitmap images such as LibreOffice or gpaint on Linux,
Microsoft Office or Windows Paint on Windows.

=head1 Events

You can send notifications and hook callbacks for the following events:

=head2 ChangeTitle

Called when the title or titleSpace gets changed

=head2 Replot

Called when the widget needs to replot "real soon", but not immediately.
Immediate replot requests should go in the form of "Paint" events.
In order to prevent the system from getting bogged down by too many
paint requests, replotting kicks off a timer that issues the paint requests
after a brief period (defaults to 30 milliseconds).

=head2 ChangeData

Called when the dataSet container changes (not the datasets themselves, but
the whole container). 

=head1 RESPONSIBILITIES

The Plot object itself has to coordinate a number of sub-systems in order to get
a functioning plot. As these responsiblities are not always clear even to their
author, here is a list of what the plot is responsible for handling.

=over

=item knowing the plot title

Although the axes are responsible for knowing the axis labels, the plot itself
is responsible for knowing the plot title and managing the space necessary for
it.

=item initiating drawing operations

The drawing of the axes, data, and plot title are all coordinated, ultimately,
by the plot object.

=item managing the dataset container

The plot does not manage the datasets directly, but it owns the dataset
container that is responsible for providing an API to the container.

=item handling user interaction

All user interaction (which at the moment is limited to mouse interaction) is
handled by the plot object. Drag and zoom events ultimately lead to changes in
the axes' minima and maxima (the axes are responsible for these pieces of
information), but these changes are initiated through the plot object.

=item file generation and clipboard interaction

Requests for raster and postscript output as well as copying the image to
the clipboard are the responsibility of the plot. 

=item initiating autoscaling

Autoscaling for either of the axes is initiated by a call to the plot object's
C<compute_min_max_for>, which computes the minima and maxima for a given axis.
Most of the calculations necessary for this operation are performed by the
dataset and the underlying plotTypes, but they are coordinated and synthesized
by the plot object.

=back

Many things are B<not> the responsiblity of the plot object but are instead
handled by other objects, which are themselves usually held by the plot.

=over

=item tick and axis details

All axis and tick details, including minima and maxima, scaling, tick mark
locations, axis labels, and conversion from real x and y data values to pixel
offsets are handled by the axis objects. (The scaling, actually, is managed
by the scaling object or class that is held by the axis.)

=item managing datasets or plotTypes

The datasets are managed by the dataset collection hashref defined in
L<PDL::Graphics::Prima::DataSet>. Any access to the datasets (outside of 
declaring them directly in the constructor, which is a special-case) is managed
through the dataSet collection. Any access to the specific plot types are 
themselves handled by the datasets themselves.

=back

=head1 TODO

This is not a perfect plotting library. Here are some of the particularly
annoying issues with it, which I hope to resolve. This is part warning to
you, gentle reade, and part task list for me.

Proper drawing of child widgets. Although subplots and insets can be added
to plot widget, they cannot be drawn to output files like postscript or
raster formats until this functionality is available.

If Prima had an SVG output, I could easily add it as a figure output option.

I have had it on my list for a while to add the facilities to turn off
drawing operations,
temporarily, so that adding a large number of dataSets can be done more
quickly. This would require some sort of interface such as

 $plot->autoupdate(0);
 ... add datasets ...
 $plot->autoupdate(1);

I have hit substantial performance problems when B<adding> over 20 datasets. 
The actual drawing of those datasets and mouse interation is fine, but the
process of just adding them to the plot can be quite sluggish.

There is essentially no way to tweak the title font, and the means for
specifying the title space is stupid and nearly useless.

The exact pixel position of the left margin depends on the size of the y-tick
labels, which can change during the process of zooming in or out. This means
mouse scroll-wheel action doesn't work exactly as advertised. Well, it does,
now that I've hedged my advertisement. Still, tracking the previous time of
a scroll wheel click and the previous x/y location could make it work
flawlessly.

There is no way to indicate by-hand where the viewport should be. It is
always calculated from the requirements of the tick labels. There is no way
to control the padding on the right side of the plot, either; it is fixed.
All of these should be tweakable.

Singular names => scalars, plural names => piddles is not consistent across
the board. At least not with all of the plotTypes. This can be fixed by
changing singular keys to plurals and quietly accepting singulars for
backwards compatibility, but it hasn't happened yet.

Multiple axes. In the constructor, any property that starts with x would be an
x-axis (x1, x2, etc). You would have to specify an axes with a dataset, though
the default would be the first axis when sorted asciibetically. Axes would have
properties regarding if they are drawn on the top, the bottom, both, etc, and
whether their tick labels are drawn on the top, bottom, etc.

I am very proud of the automatic scaling. Unfortunately, it's complicated
and not yet well documented. Also, it could be even more awesome. It needs to
allow for negative pixel paddings as well as "extra" pixel padding. This
would simply effect how collate_min_max_for_many works and should be a snap
to implement. For what it's worth, the collation code should probably be
rewritten in C.

Automatic scaling should allow for 'next widest tick' in addition to
the current super-tight bounds that it calculates. This would make hard-copy
figures much, much nicer.

=head1 SEE ALSO

Both the L<Perl Data Language|PDL> and the L<Prima GUI Toolkit|Prima> are
amazing and this module would have no reason for existence without both of them.

This module serves as the motivation for L<PDL::Drawing::Prima>, and also would
be unable to function with any efficiency without it.

Other 2D plotting options include L<PDL::Graphics::PGPLOT>,
L<PDL::Graphics::PLplot>, L<PDL::Graphics::Gnuplot>. There may be a few
others. For my part, I also wrote L<PDL::Graphics::Asymptote>, though it is
more of a toy than these other libraries. Search CPAN for more.

For 3D plotting, see PDL's own L<PDL::Graphics::TriD>, as well as
L<PDL::Graphics::Gnuplot> and the low-level bindings in
L<PDL::Graphics::PLplot>.

=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 ADDITIONAL MODULES

Here is the full list of modules in this distribution:

=over

=item L<PDL::Graphics::Prima|PDL::Graphics::Prima/>

Defines the Plot widget for use in Prima applications

=item L<PDL::Graphics::Prima::Axis|PDL::Graphics::Prima::Axis/>

Specifies the behavior of axes (but not the scaling)

=item L<PDL::Graphics::Prima::DataSet|PDL::Graphics::Prima::DataSet/>

Specifies the behavior of DataSets

=item L<PDL::Graphics::Prima::Limits|PDL::Graphics::Prima::Limits/>

Defines the lm:: namespace

=item L<PDL::Graphics::Prima::Palette|PDL::Graphics::Prima::Palette/>

Specifies a collection of different color palettes

=item L<PDL::Graphics::Prima::PlotType|PDL::Graphics::Prima::PlotType/>

Defines the different ways to visualize your data

=item L<PDL::Graphics::Prima::ReadLine|PDL::Graphics::Prima::ReadLine/>

Encapsulates all interaction with the L<Term::ReadLine> family of
modules.

=item L<PDL::Graphics::Prima::Scaling|PDL::Graphics::Prima::Scaling/>

Specifies different kinds of scaling, including linear and logarithmic

=item L<PDL::Graphics::Prima::Simple|PDL::Graphics::Prima::Simple/>

Defines a number of useful functions for generating simple and not-so-simple
plots

=back

=head1 LICENSE AND COPYRIGHT

Portions of this module's code are copyright (c) 2011 The Board of Trustees at
the University of Illinois.

Portions of this module's code are copyright (c) 2011-2013 Northwestern
University.

This module's documentation are copyright (c) 2011-2013 David Mertens.

All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
