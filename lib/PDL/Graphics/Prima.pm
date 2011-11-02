use strict;
use warnings;
$| = 1;

package Prima::Plot;
use PDL::Lite;
use Prima;
use Prima::ImageDialog;
use Prima::MsgBox;
use Prima::Utils;

use base 'Prima::Widget';

use Carp qw(croak cluck confess);
use PDL::NiceSlice;
use PDL::Drawing::Prima;

# I will need these graph-specific modules, too:
use PDL::Graphics::Prima::Limits;
use PDL::Graphics::Prima::Axis;
use PDL::Graphics::Prima::DataSet;
use PDL::Graphics::Prima::BoundsDialog;

=head1 NAME

PDL::Graphics::Prima - an interactive graph widget for PDL and Prima

=head1 SYNOPSIS

 use strict;
 use warnings;
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
     -function => [\&PDL::exp, color => cl::Blue],
     -data => [$t_data, $y_data, color => cl::Red],
     pack => { fill => 'both', expand => 1},
 #   y => {scaling => sc::Log},
 );
 
 run Prima;

=head1 OVERVIEW

Here is an overview of the plotting infrastructure to help keep your head
straight. The data types are indicated after the datatype and information
that is only meant to be used internally is in parentheses

At the moment, it is not quite accurate and needs updating. I'm Sory. :-(

 Plotting Widget
  |- xLabel string
  |- yLabel string
  |- title string
  |- backColor colorValue
  |- replotDuration float in milliseconds
  |- x and y axes
    |- min float
    |- max float
    |- lowerEdge integer
    |- upperEdge integer
    |- scaling, a class name or an object
      |- $self->compute_ticks($min, $max)
      |- $self->transform($min, $max, $data)
      |- $self->inv_transform($min, $max, $data)
      |- $self->sample_evently($min, $max, $N_values)
      |- $self->is_valid_extremum($value)
    |- (minValue float)
    |- (minAuto boolean)
    |- (maxValue float)
    |- (maxAuto boolean)
#   |- (pixel_extent int)
#   |- $self->pixel_extent([$new_extent])
?   |- $self->recompute_min_auto()
?   |- $self->recompute_max_auto()
    |- $self->update_edges()
?   |- $self->minmax_with_padding($data)
    |- $self->reals_to_relatives($data)
    |- $self->relatives_to_reals($data)
    |- $self->pixels_to_relatives($data)
    |- $self->relatives_to_pixels($data)
    |- $self->reals_to_pixels($data)
    |- $self->pixels_to_reals($data)
  |- dataSets (name => data)
    |- xs (floats)
    |- ys (floats)
    |- plotType
      |- type-specific data
      |- $self->xmin($dataset, $widget)
      |- $self->xmax($dataset, $widget)
      |- $self->ymin($dataset, $widget)
      |- $self->ymax($dataset, $widget)
      |- $self->draw($dataset, $widget)
    |- $self->get_data_as_pixels($widget)
    |- $self->extremum($nane, $comperator, $widget)
  |- $self->compute_min_max_for($axis_name)


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
	$self->x->update_edges;
	$self->y->update_edges;
}

my $inf = -PDL->new(0)->log->at(0);

=for details
XXX working here
XXX see also: Axis.pm recompute_max_auto, recompute_min_auto

=for motivation
The major issue with determining automatic scaling is that I consider two
distinct units of measure, the scale of the data being one of them and the other
being screen pixels. Furthermore, large padding on one side can impact the
scaling on the other. Determining the correct min and max so that the pixel
padding gets respected is, to the best of my knowledge, not a simple matter of 
linear algebra.

=for first-naive-implementation
The first naive implementation, which was the implementation I used as my first
shot at solving the problem of automatic scaling, is to get the min/max of the
data, as well as the min/max padding. You do this for all the datasets and then
take the most extreme values as your guess. The problem with this approach is
that it could lead to overestimates of the extrema (i.e. guesses that are too
wide), leading to plots that are not ideal. For example, suppose you have two
datasets, one being a line plot with a wide range and the other being a blob
plot with a very narrow range but a large blob size (i.e. 40 pixels). Using this
method, you would allow for a 40-pixel padding on the most extreme data for the
line plot, which could lead to extra and unnecessary white space. However, it is
quite fast compared with the second naive implementation. The complexity for
this method is about O(n), where n is the number of data points.

=for second-naive-implementation
The second naive implementation is an iterative approach in which you guess at
the min and max that will display all of the data and plot types. You then run
through all the data points and see if any of them do not fit within the
min/max. If you find anything that doesn't fit, you widen your bounds and repeat
the search. Although the whitespace padding would be correctly computed using
this algorithm, this method is computationally inefficient and could be terribly
slow for very large datasets. In the worst case, I believe that this algorithm
would be O(n**2) or maybe even O(n**3), or it would make use of data structures
of size O(n).

=for better-implementation-analysis
My proposed algorithm is a sort of combination of both naive implementations. It
is slower than the first naive implementation but will almost always be as fast
as or faster than the second naive implementation and with much, much better
scaling properties. For large datasets, the computational complexity goes as
O(n) while keeping the memory footprint small. However, it should give the
bounds with accuracy as good as the second naive implementation and much better
than the worst-case bounds using the first method.

=for better-implementation-overview
Upon inspetion of the second naive implementation, it becomes clear that we can
greatly reduce the amount of time spent checking our guesses of the min and the
max by noting that we only need to keep track of the most extreme values for a 
given amount of padding. In other words, if we have many data points that need a
padding of 10 pixels, we only need to keep track of their minimum and maximum
values. For example, suppose we have three blobs all with radii 10 pixels and
with x-values of 10, 12, 13, and 17. We know that if we determine a plotting
minimum that can accomodate the left edge of point with x = 10, the certainly
the point at x = 12 will fit within that minimum because we know it has the same
padding. Similarly, we only need to know that the maximum x-value of the set is
17. If we can accomodate a max of 17, the others will certainly fit within those
bounds. Coming back to the implementation, I can scan through all the data
keeping track of the minimum and maximum values for each value of pixel-padding.
That is, I keep track of the min and max x-values for a padding of 10 pixels,
and I seperately track the min and max x-values for a padding of 11, 12, 20, or
200 pixels as they arise. In the end, I have no more than a few hundred extrema
for different pixel paddings that I need to combine properly. Since I could
potentially try to plot millions of data points, this reduces the amount of data
that needs to be processed from millions of data points to only a few hundred.

=for better-implementation-collection
The better implementation works as follows. First choose a maximum padding that
you care about for the purposes of determining the scaling. 500 pixels seems
like a reasonable number but 2000 is just as feasable for the purposes of the
algorithm. Allocate two arrays with as many elements for the min and max values,
respectively. Then run through all the datasets. For each data point, get that
point's requested pixel padding as well as its value. Use the padding value as
the array offset and look up the currently known minimum and maximum values for
that pixel padding. If the point is more extreme, replace the old extremum with
the current value. This is only slightly slower than computing the min/max
values required in both naive implementations, and requires very little memory.

=for better-implementation-first-pruning
The next step is optional but will likely speed-up the iterative process. It is
likely that the plot will only have a handful of pixel-paddings, so running
through all 500 (or however many you allocated) is a waste of time. As such, the
next step of the process is to find the largest pixel padding representated in
the collection and then find all smaller pixel padding values for which the
extremum is more extreme than the extrema of the higher paddings. In the end,
you have a collection of extrema which you can think of as being in a pyramid:
the lowest pixel-padding is associated with the largest extremum, and the
highest pixel-padding is associated with the least extreme value.

=for better-implementation-iterating
XXX working here

Work with arrays, in which case the index itself is equal to the needed
padding. Build a doubly-linked list with a structure patterned after

 padding => number    # padding of interest
 data => float        # min/max for this padding
 curr_value => float  # computed extent
 next => pointer      # next (smaller) padding

Also, keep track of the tail, the current min, and the current max.

The linked list is initially assembled in order of decreasing padding
(largest padding on top). Here's something that's important, which you will
need to get your head around, and which I will illustrate with an example.
Suppose we have two paddings, 10 pixels and 5 pixels, and we're trying to
compute the minimum. If the minimum data value with a pixel padding of 5 is
2.2 and the minimum data value with a pixel padding of 10 is 2.1, we know
that the pixel padding of 10 must lead to a smaller minimum than the pixel
padding of 5. As such, we can remove the pixel padding of 5 from the
list. I call this weeding out the values. The result is that as we go
through the list in order of decreasing padding, the data values will become
more extreme, like a pyramid.

The argument I just made about the paddings for 5 and 10 pixels only took
their data values and the sort order of the padding into account. It did not
take the actual values of the paddings into account. In the next stage,
which is iterative, I will begin to account for the effect of the different
padding values.

With the pyramid in hand, examine the tail values for both the min and the
max. Each of these will have a padding associated with them. Estimate the
min and max by assuming that the tail values, together with their padding,
represent the most extreme values of the data set, which is a conservative
estimate. With this estimate in hand, run through the list and compute the
min or max associated with each list element, taking the padding and current
scaling into account, and storing the result in curr_value. Then weed out
the list using curr_value and iterate the procedure of this last paragraph
until the tail of both the min and the max lists does not change.

An important feature of this algorithm is that the min/max values begin with
very conservative estimates and become more extreme with each round.

Furthermore, the pyramid data structure is arranged so that with each round
the width of the top end of the pyramid grows more than the width of the
bottom end.

To actually implement this scheme, I will require that datasets monitor
their own data and report a data/padding list upon request. How the datasets
monitor their data is entirely up to them. (I am considering using an
on_change slice, which I secretly insert over the user's piddle by modifying
the @_ argument array, to efficiently monitor changes.)

In order to properly handle bad values, I need to write a function that can
look for bad values over many piddles, tens of piddles. I believe I can
achieve this by writing a function that takes, say, 20 piddles, and wrapping
it in Perl code that supplies null piddles when you only need to call the
function for 10 piddles. The funcion would be called collate_min_max_for_many
and the calling convention for it would look like this:

 ($min, $max) = collate_min_max_for_many(N_to_return, N_buckets, $index, $p1, $p2, ...)

This is best illustrated with the blobs plot type, since it would use a
nontrivial value for index. If I wanted to compute the collated min and max
for the x-data, taking potential bad values for y, xradii, yradii, and
colors into account, I would call the function like so:

 my ($blob_x_min, $blob_x_max)
     = collate_min_max_for_many(
         1,              # return min/max for $x
         $widget->width, # only need the number of pixels corresponding to the widget
         $xRadii,        # the index
         $x,             # x-data for which to find min/max
         $y,             # \
         $yRadii,        #  |- ignore x-values if any of these are bad
         $colors,        # /
     )
         

In this case

where N <= M, M < 20, and the return dimensions are N x whatever.

=cut

sub compute_min_max_for {
	my ($self, $axis_name) = @_;
	
	# Get plotting pixel extent, which I'll need to send to the dataSets in
	# order for them to compute their pyramids.
	my ($left, $bottom, $right, $top) = $self->get_edge_requirements;
	my $pixel_extent;
	if ($axis_name eq 'x') {
		$pixel_extent = $self->width - $left - $right;
	}
	else {
		$pixel_extent = $self->height - $top - $bottom;
	}
	
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
	
	# Iterativelye pair down the set until we've found the minmax. At this
	# point, we have two arrays with $pixel_extent elements each. Cat an
	# index and what will eventually be a computed value onto the original
	# lists so our slicing keeps track of the padding and original values.
	my $minima = $collated_min->cat($collated_min->sequence, $collated_min);
	my $maxima = $collated_max->cat($collated_max->sequence, $collated_max);

	# I should check this for sanity, like if none of the min or max are
	# good. (In that case, I think they should both fail.) working here
	my $trimmed_minima = $minima->whereND($minima(:,0;-)->isgood);
	my $trimmed_maxima = $maxima->whereND($maxima(:,0;-)->isgood);

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

=head2 xLabel, yLabel, title

Sets or gets the various strings for the axis labeling and the title.

=cut

# Setter that does not notify and sets the title spacing property as well
# The constructor (init) must call this function, or initialize titleSpace
# itself.
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
	
	# Finish by issuing a notification:
	$_[0]->notify('ChangeData');
}

# For a change in title, recompute the autoscaling and replot.
# Make sure this 
sub on_changetitle {
	my $self = shift;
	$self->x->update_edges;
	$self->y->update_edges;
	$self->notify('Replot');
}

# Sets up a timer in self that eventually calls the paint notification:
sub on_replot {
	my ($self) = @_;
	return if $self->{timer}->get_active;
	$self->{timer}->start;
}

=head1 Events

You can send notifications and tie callbacks for the following events:

=head2 ChangeTitle

=head2 Replot

=head2 ChangeData

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
		map { ("Change$_" => nt::Default) } qw(Title Data),
	);
	
	sub notification_types { return \%notifications }
}

sub on_paint {
	my ($self) = @_;
	
	# Clear the canvas:
	$self->clear;
	
	# Get the clipping rectangle for the actual drawing space:
	my ($clip_left, $clip_bottom, $right_edge, $top_edge)
		= $self->get_edge_requirements;
	
	# The right and top edge values should be subtracted from the width and
	# height, respectively:
	my $clip_right = $self->width - $right_edge;
	my $clip_top = $self->height - $top_edge;
	
	# Clip the widget before we begin drawing
	$self->clipRect($clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# backup the drawing parameters:
	# working here - consider writing PDL::Drawing::Prima to back these up
	# for me automatically so I don't have to do it here
	# also, consider a better way than listing them here explicitly
	my @to_backup = qw(color backColor linePattern lineWidth lineJoin
			lineEnd rop rop2);
	my %backups = map {$_ => $self->$_} (@to_backup);
	
	# Draw the data, sorted by key name:
	foreach my $key (sort keys %{$self->{dataSets}}) {
		next if $key eq 'widget';
		my $dataset = $self->{dataSets}->{$key};
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
	$self->x->draw($self, $clip_left, $clip_bottom, $clip_right, $clip_top);
	$self->y->draw($self, $clip_left, $clip_bottom, $clip_right, $clip_top);
	
	# Draw the title:
	if ($self->{titleSpace}) {
		my ($width, $height) = $self->size;
		# Set up the font characteristics:
		my $font_height = $self->font->height;
		$self->font->height($font_height * 1.5);
		my $style = $self->font->style;
		$self->font->style(fs::Bold);
		
		# Draw the title:
		$self->draw_text($self->{title}, 0, $height - $self->{titleSpace}
				, $width, $height
				, dt::Center | dt::VCenter | dt::NewLineBreak | dt::NoWordWrap
				| dt::UseExternalLeading);
		
		# Reset the font characteristics:
		$self->font->height($font_height);
		$self->font->style($style);
	}
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

use Time::HiRes;
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
			my @items = (
					['~Edit Bounds...' => sub { self->edit_bounds }],
					['~Save As...' => sub {
							# Sleep for a quarter-second to clear the menu:	
							Prima::Utils::post(\&Time::HiRes::usleep, 250_000);
							Prima::Utils::post(\&save_to_file, $self)
					}],
					['~Autoscale' => sub {
							$self->x->minmax(lm::Auto, lm::Auto);
							$self->y->minmax(lm::Auto, lm::Auto);
					}],
			);
			# working here - add context-dependent items
			my $popup = Prima::Popup->new(items => \@items);
		}
		# Remove the previous button record, so a zoom rectangle is not drawn:
		delete $self->{mouse_down_rel}->{mb::Right};
	}
}

# A routine to save the current plot to a rasterized file:
sub save_to_file {
	# Get the filename as an argument or from a save-as dialog.
	my ($self, $filename) = @_;
	
	# Get the image
	my $image = $::application->get_image($self->client_to_screen($self->origin), $self->size);
	
	# If they didn't specify a filename, run a dialog to get it:
	unless ($filename) {
		my $dlg = Prima::ImageSaveDialog-> create;
	
		$dlg->save($image);
		return;
	}
	
	# If they specified a filename, simply save it:
	$image-> save($filename) or
		Prima::MsgBox::message("Unable to save plot to '$filename'", mb::Ok);
}

# A routine that pops up a modal dialog box for editing the plot's bounds:
sub edit_bounds {
	$current_plot_widget = shift;
	my $result = PDL::Graphics::Prima::BoundsDialog->run($current_plot_widget);
}

1;
