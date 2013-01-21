use strict;
use warnings;
use PDL::Graphics::Prima::PlotType;
use PDL::Graphics::Prima::Palette;

# Codifies the different kinds of dataset plotting that you can do, and defines
# the class for the tied dataset array.

=head1 NAME

PDL::Graphics::Prima::DataSet - the way we think about data

=head1 SYNOPSIS

 -distribution => ds::Set(
     $data, plotType => pset::CDF
 ),
 -lines => ds::Pair(
     $x, $y, plotType => [ppair::Lines, ppair::Diamonds]
 ),
 -contour => ds::Grid(
     $matrix, bounds => [$left, $bottom, $right, $top],
              y_edges => $ys, x_bounds => [$left, $right],
              x_edges => $xs, y_bounds => [$bottom, $top],
              plotType => pgrid::Matrix(palette => $palette),
 ),
 -image => ds::Image(
     $image, format => 'string',
             ... ds::Grid bounder options ...
             plotType => pimage::Basic,
 ),
 -function => ds::Func(
     $func_ref, xmin => $left, xmax => $right, N_points => 200,
 ),
 -func_grid => ds::FGrid(
     $matrix, ... same as for ds::Grid ...
              N_points => 200,
              N_points => [200, 300],
 ),
 

=head1 DESCRIPTION

C<PDL::Graphics::Prima> differentiates between a few kinds of data: Sets,
Pair collections, and Grids. A Set is an unordered collection of data, such as the
heights of a class of students. A Pair collection is an collection of x/y
pairs, such as a time series. A Grid is, well, a matrix, such as the pixel
colors of a photograph.

working here - this needs to be cleaned up!

In addition, there are two derived kinds of datasets when you wish to specify
a function instead of raw set of data. For example, to plot an analytic function,
you could use a Function instead of Pairs. This has the advantage that if
you zoom in on the function, the curve is recalculated and looks smooth instead
of jagged. Similarly, if you can describe a surface by a function, you can plot
that function using a function grid, i.e. FGrid.

Once upon a time, this made sense, but it needs to be revised:

 At the moment there are two kinds of datasets. The piddle-based datasets have
 piddles for their x- and y-data. The function-based datasets create their
 x-values on the fly and evaluate their y-values using the supplied function
 reference. The x-values are generated using the C<sample_evenly> function which
 belongs to the x-axis's scaling object/class. As such, any scaling class needs
 to implement a C<sample_evenly> function to support function-based datasets.

=head2 Base Class

The Dataset base class provides a few methods that work for all datasets.
These include accessing the associated widget and drawing the data.

=over

=cut

package PDL::Graphics::Prima::DataSet;
use Carp;

=item widget

The widget associated with the dataset.

=cut

sub widget {
	# Simply return the widget if it's a getter:
	return $_[0]->{widget} if @_ == 1;
	
	# It's a setter call, so set self's widget
	my ($self, $widget) = @_;
	$self->{widget} = $widget;
}

=item draw

Calls all of the drawing functions for each plotType of the dataSet. This also
applies all the global drawing options (like C<color>, for example) that were
supplied to the dataset.

=cut

# Calls all the drawing functions for the plotTypes for this dataset:
sub draw {
	my ($dataset, $canvas, $ratio) = @_;
	my $widget = $dataset->widget;
	
	# Call each plot type's drawing function, in the order specified:
	foreach my $plotType (@{$dataset->{plotTypes}}) {
		$plotType->draw($canvas, $ratio);
	}
}

=item compute_collated_min_max_for

This function is part of the collated min/max chain of function calls that
leads to reasonable autoscaling. The Plot widget asks each dataSet to determine
its collated min and max by calling this function, and this function simply
agregates the collated min and max results from each of its plotTypes.

In general, you needn't worry about collated min and max calculations unless
you are trying to grapple with the autoscaling system or are creating a new
plotType.

working here - link to more complete documentation of the collation and
autoscaling systems.

=cut

# working here - function based datasets with nonzero widths will get
# their x-bounds from the widget, then return wider bounds to accomodate
# the data for the full width. This means that either (1) successive
# resizing will lead to larger and larger bounds or (2) the plots of the
# function-data will be cropped. Of course, all of this is only a problem
# with auto-scaling. Otherwise it gives no trouble.

sub compute_collated_min_max_for {
	# Must get the collated min max for each plot type for this data:
	my ($self, $axis_name, $pixel_extent) = @_;
	my $widget = $self->{dataSets}->{widget};
	
	my (@min_collection, @max_collection);
	foreach my $plotType (@{$self->{plotTypes}}) {
		
		# Accumulate all the collated results
		my ($min, $max)
		= $plotType->compute_collated_min_max_for($axis_name, $pixel_extent);
		# The collated results are not required to be one dimensional.
		# As such, I need to reduce them. I do this by moving the dimension
		# with $pixel_extent entries to the back and then calling minimum
		# until I have only one dimension remaining.
		warn "Internal error: plot type $plotType returned an undefined min"
			if not defined $min;
		warn "Internal error: plot type $plotType returned an undefined max"
			if not defined $max;
		next if not defined $min or not defined $max;
		$min = $min->squeeze->mv(0,-1);
		$min = $min->minimum while($min->ndims > 1);
		$max = $max->squeeze->mv(0,-1);
		$max = $max->maximum while($max->ndims > 1);
		push @min_collection, $min;
		push @max_collection, $max;
	}
	
	# Merge all the data:
	my $collated_min = PDL::cat(@min_collection)->mv(-1,0)->minimum;
	my $collated_max = PDL::cat(@max_collection)->mv(-1,0)->maximum;
	
	return ($collated_min, $collated_max);
}

=item new

This is the universal constructor that is called by the short-name constructors
introduced below. This handles the uniform packaging of plotTypes (for
example, allowing the user to say C<plotType => ppair::Diamonds> instead of
the more verbose C<plotTypes => [ppair::Diamonds]>). In general, you (the
user) will not need to invoke this constructor directly.

=cut

sub new {
	# Short-name constructors are required to send us stuff that looks like
	# this. In particular, they must pack their data into appropriately
	# named keys among the %options
	my ($class, %options) = @_;
	
	my $self = \%options;
	bless ($self, $class);
	
	# Ensure that we have a plural plotTypes key, and no singular plotType key:
	if (exists $self->{plotType}) {
		my $type = $self->{plotType};
		if (ref($type) eq 'ARRAY') {
			$self->{plotTypes} = $type;
		}
		else {
			$self->{plotTypes} = [$type];
		}
		delete $self->{plotType};
	}
	
	# Ensure that if they supplied a plotTypes, that it is an array reference,
	# not a singulare plotType:
	$self->{plotTypes} = [$self->{plotTypes}]
		if (exists $self->{plotTypes} and ref($self->{plotTypes}) ne 'ARRAY');
	
	# Initialize (and validate) the dataSet
	$self->init;
	
	# Make sure that the plotTypes know which dataSet they belong to:
	for my $plotType (@{$self->{plotTypes}}) {
		$plotType->dataset($self);
	}
	
	return $self;
}


=item check_plot_types

Checks that the plotType(s) passed to the constructor or added at runtime are
built on the data type tha we expect. Derived classes must specify their
C<plotType_base_class> key before calling this function.

=cut

sub check_plot_types {
	my ($self, @plotTypes) = @_;
	my $expected_plot_class = $self->expected_plot_class;
	foreach (@plotTypes) {
		croak("Plottype should be of type $expected_plot_class, but it is of type "
			. ref($_)) unless eval{$_->isa($expected_plot_class)}
						or eval{$_->isa('PDL::Graphics::Prima::PlotType::CallBack')}
							and eval{$_->{base_class}->isa($expected_plot_class)};
	}
}

=item init

Called by new to initialize the dataset. This function is called on the new
dataset just before it is returned from C<new>.

If you create a new dataSet, you should provide an C<init> function that
performs the following:

=over

=item supply a default plotType

If the user supplied something to either the C<plotType> or C<plotTypes> keys,
then C<new> will be sure you have you will already have that something in
an array reference in C<< $self->{plotTypes} >>. However, if they did not supply
either key, you should supply a default. You should have something that looks
like this:

 $self->{plotTypes} = [pset::CDF] unless exists $self->{plotTypes};

=item check the plot types

After supplying a default plot type, you should check that the provided plot
types are derived from the acceptable base plot type class. You would do this
with code like this:

 $self->check_plot_types(@{self->{plotTypes}});

=back

This is your last step to validate or pre-calculate anything. For example, you
must provide functions to return your data, and you should probably make
guarantees about the kinds of data that such accessors return, such as the data
always being a piddle. If that is the case, then it might not be a bad idea to
say in your C<init> function something like this:

 $self->{data} = PDL::Core::topdl($self->{data});

=cut

sub init {
	my $self = shift;
	die "The class for $self does not define an init function!";
}

=item change_data

Sets the data to the given data by calling the derived class's C<_change_data>
method. Unlike C<_change_data>, this method also issues a C<ChangeData>
notification to the widget. This means that you should only use this method
once the dataset has been associated with a widget. Each class expects
different arguments, so you should look at the class's documentation for
details on what to send to the method.

=cut

# Sets the data and issues a notification
sub change_data {
	my $self = shift;
	$self->_change_data(@_);
	$self->widget->notify('ChangeData', $self);
}

=back

=cut

################################################################################
#                                     Sets                                     #
################################################################################

package PDL::Graphics::Prima::DataSet::Set;
use base 'PDL::Graphics::Prima::DataSet';
use Carp;

=head2 Sets

Sets are unordered collections of sample data. The typical use case of set data
is that you have a population of things and you want to analyze their agregate
properties. For example, you might be interested in the distribution of tree
heights at your Christmas Tree Farm, or the distribution of your students' (or
your classmates') test scores from the mid-term. Those collections of data are
called Sets and PDL::Graphics::Prima provides a number of ways of visualizing
sets, as discussed under L<PDL::Graphics::Prima::PlotType/Sets>. Here, I
discuss how to create and manipulate Set dataSet objects.

Note that shape of pluralized properties (i.e. C<colors>) should
thread-match the shape of the data B<excluding> the data's first dimension.
That is, if I want to plot the cumulative distributions for three different
batches using three different line colors, my data would have shape (N, 3) and
my colors piddle would have shape (3).

=over

=item ds::Set - short-name constructor

=for sig

    ds::Set($data, option => value, ...)

The short-name constructor to create sets. The data can be either a piddle of
values or an array reference of values (which will be converted to a piddle
during initialization).

=cut

sub ds::Set {
	croak('ds::Set expects data and then key => value pairs, but you supplied'
		. ' an even number of arguments') if @_ % 2 == 0;
	my $data = shift;
	return PDL::Graphics::Prima::DataSet::Set->new(@_, data => $data);
}

=item expected_plot_class

Sets expect plot type objects that are derived from 
C<PDL::Graphics::Prima::PlotType::Set>.

=cut

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Set'}

# Standard initialization, and ensures that the data is a piddle.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [pset::CDF()] unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Set the data (without notifications)
	$self->_change_data($self->{data});
}

=item get_data

Returns the piddle containing the data. This is used mostly by the plotTypes to
retrieve the data in order to display it. You can also use it to retrieve the
data piddle if it makes your code more legible.

=for example

 my $heights = load_height_data();
 ...
 my $plot = $wDisplay->insert('Plot',
     -heights => ds::Set($heights),
     ...
 );
 
 # Retrieve and print the data:
 print "heights are ", $plot->dataSets->{heights}, "\n";

A subtle point: notice that you can change the data B<within> the piddle, and
you can even change the piddle's shape, but you cannot use this to replace the
piddle itself.

=cut

sub get_data {
	return $_[0]->{data};
}

=item change_data

Changes the data to the piddle passed in. For example,

 if ($heights->changed) {
     my $new_heights = $heights->data;
     $heights_plot->dataSets->{'heights'}->change_data($new_heights);
 }

=cut

# Notification free method
sub _change_data {
	my ($self, $data) = @_;
	
	# Ensure the data is a piddle:
	eval {
		$self->{data} = PDL::Core::topdl($data);
		1;
	} or croak('For Set datasets, the data must be piddles '
		. 'or scalars that pdl() knows how to process');
}

=back

=cut

###############################################################################
#                                    Pair                                    #
###############################################################################

package PDL::Graphics::Prima::DataSet::Pair;
use base 'PDL::Graphics::Prima::DataSet';
use Carp 'croak';

=head2 Pair

Pairwise datasets are collections of paired x/y data. A typical Pair dataset
is the sort of thing you would visualize with an x/y plot: a time series
such as the series of high temperatures for each day in a month or the x- and
y-coordinates of a bug walking across your desk. PDL::Graphics::Prima provides
many ways of visualizing Pair datasets, as discussed under
L<PDL::Graphics::Prima::PlotType/Pair>.

The dimensions of pluralized properties (i.e. C<colors>) should
thread-match the dimensions of the data. An important exception to this is
C<ppair::Lines>, in which case you must specify how you want properties to thread.

The default plot type is C<ppair::Diamonds>.

=over

=item ds::Pair - short-name constructor

=for sig

    ds::Pair($x_data, $y_data, option => value, ...)

The short-name constructor to create pairwise datasets. The x- and y-data
can be either piddles or array references (which will be converted to a
piddle during initialization).

=cut

sub ds::Pair {
	croak('ds::Pair expects x- and y-data and then key => value pairs, but you'
		. ' supplied an odd number of arguments') if @_ % 2 == 1;
	my ($x_data, $y_data) = (shift, shift);
	return PDL::Graphics::Prima::DataSet::Pair->new(@_
		, x => $x_data, y => $y_data);
}

=item expected_plot_class

Pair datasets expect plot type objects that are derived from
C<PDL::Graphics::Prima::PlotType::Pair>.

=cut

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Pair'}

# Standard initialization, and ensures that the data are piddles.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [ppair::Diamonds(filled => 1)]
		unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Set the data
	$self->_change_data($self->{x}, $self->{y});
}

=item get_xs, get_ys, get_data

Returns piddles with the x, y, or x-y data. The last function returns two
piddles in a list.

=cut

sub get_xs { $_[0]->{x} }
sub get_ys { $_[0]->{y} }
# This is really a convenience function that wraps the other two. However,
# it really wraps them, so that to override the behavior you simply need to
# override get_xs and get_ys.
sub get_data {
	my $self = shift;
	return ($self->get_xs, $self->get_ys);
}

=item get_data_as_pixels

Uses the reals_to_pixels functions for the x- and y- axes to convert the
values of the x- and y- data to actual pixel positions in the widget.

=cut

sub get_data_as_pixels {
	my ($dataset, $ratio) = @_;
	my $widget = $dataset->widget;
	
	my ($xs, $ys) = $dataset->get_data;
	return ($widget->x->reals_to_pixels($xs, $ratio), $widget->y->reals_to_pixels($ys, $ratio));
}

=item change_data

Changes the data to the piddles passed in. For example,

 $scatter_plot->dataSets->{'data'}->change_data($xs, $ys);

=cut

sub _change_data {
	my ($self, $x, $y) = @_;
	
	# Ensure the data are piddles:
	eval {
		$self->{x} = PDL::Core::topdl($x);
		$self->{y} = PDL::Core::topdl($y);
		1;
	} or croak('For pairwise datasets, the x- and y-data must be piddles '
		. 'or scalars that pdl() knows how to process');
}

=back

=cut

# working here - create an OrderedSet, which only requires a single set of
# data and uses counting numbers for the "x" values

################################################################################
#                                     Grid                                     #
################################################################################

package PDL::Graphics::Prima::DataSet::Grid;
use base 'PDL::Graphics::Prima::DataSet';
use Carp 'croak';
use PDL;

=head2 Grids

Grids are collections of data that is regularly ordered in two dimensions. Put
differently, it is a structure in which the data is described by two indices.
The analogous mathematical structure is a matrix and the analogous visual is an
image. PDL::Graphics::Prima provides a few ways to visualize grids, as
discussed under L<PDL::Graphics::Prima::PlotType/Grids>. The default plot type
is C<pgrid::Color>.

This is the least well thought-out dataSet. As such, it may change in the
future. All such changes will, hopefully, be backwards compatible.

At the moment, there is only one way to visualize grid data: C<pseq::Matrix>.
Although I can conceive of a contour plot, it has yet to be implemented. As
such, it is hard to specify the dimension requirements for dataset-wide
properties. There are a few dataset-wide properties discussed in the
constructor, however, so see them for some examples.

=over

=item ds::Grid - short-name constructor

=for sig

    ds::Grid($matrix, option => value, ...)

The short-name constructor to create grids. The data should be a piddle of
values or something which topdl can convert to a piddle (an array reference of
array references).

The current cross-plot-type options include the bounds settings. You can either
specify a C<bounds> key or one key from each column:

 x_bounds   y_bounds
 x_centers  y_centers
 x_edges    y_edges

=over

=item bounds

The value associated with the C<bounds> key is a four-element anonymous array:

 bounds => [$left, $bottom, $right, $top]

The values can either be scalars or piddles that indicate the corners of the
grid plotting area. If the latter, it is possible to thread over the bounds by
having the shape of (say) C<$left> thread-match the shape of your grid's data,
excluding the first two dimensions. That is, if your C<$matrix> has
a shape of (20, 30, 4, 5), the piddle for C<$left> can have shapes of (1), (4),
(4, 1), (1, 5), or (4, 5).

At the moment, if you specify bounds, linear spacing from the min to the max is
used. In the future, a new key may be introduced to allow you to specify the
spacing as something besides linear.

=item x_bounds, y_bounds

The values associated with C<x_bounds> and C<y_bounds> are anonymous arrays with
two elements containing the same sorts of data as the C<bounds> array.

=item x_centers, y_centers

The value associated with C<x_centers> (or C<y_centers>) should be a piddle with
increasing values of x (or y) that give the mid-points of the data. For example,
if we have a matrix with shape (3, 4), C<x_centers> would have 3 elements and
C<y_centers> would have 4 elements:

    -------------------
 y3 | d03 | d13 | d23 |
    -------------------
 y2 | d02 | d12 | d22 |
    -------------------
 y1 | d01 | d11 | d21 |
    -------------------
 y0 | d00 | d10 | d20 |
    -------------------
      x0    x1    x2

Some plot types may require the edges. In that case, if there is more than one
point, the plot guesses the scaling of the spacing between points (choosing
between logarithmic or linear) and appropriate bounds for the given scaling are
calculated using interpolation and extrapolation. The plot will croak if there
is only one point (in which case interpolation is not possible). If the spacing
for your grid is neither linear nor logarithmic, you should explicitly specify
the edges, as discussed next.

At the moment, the guess work assumes that all the scalings for a given Grid
dataset are either linear or logarithmic, even though it's possible to mix
the scaling using threading. (It's hard to do that by accident, so if that last
bit seems confusing, then you probably don't need to worry about tripping on
it.) Also, I would like for the plot to croak if the scaling does not appear to
be either linear or logarithmic, but that is not yet implemented.

=item x_edges, y_edges

The value associated with C<x_edges> (or C<y_edges>) should be a piddle with
increasing values of x (or y) that give the boundary edges of data. For example,
if we have a matrix with shape (3, 4), C<x_edges> would have 3 + 1 = 4 elements
and C<y_edges> would have 4 + 1 = 5 elements:

 y4 -------------------
    | d03 | d13 | d23 |
 y3 -------------------
    | d02 | d12 | d22 |
 y2 -------------------
    | d01 | d11 | d21 |
 y1 -------------------
    | d00 | d10 | d20 |
 y0 -------------------
    x0    x1    x2    x3

Some plot types may require the data centers. In that case, if there are only
two edges, a linear interpolation is used. If there are more than two points,
the plot will try to guess the spacing, choosing between linear and logarithmic,
and use the appropriate interpolation.

The note above about regarding guess work for x_centers and y_centers applies
here, also.

=back

=cut

sub ds::Grid {
	croak('ds::Grid expects data and then key => value pairs, but you'
		. ' supplied an even number of arguments') if @_ % 2 == 0;
	my $data = shift;
	return PDL::Graphics::Prima::DataSet::Grid->new(@_, data => $data);
}

=item expected_plot_class

Grids expect plot type objects that are derived from
C<PDL::Graphics::Prima::PlotType::Grid>.

=cut

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Grid'}
sub default_plot_type { pgrid::Matrix() }

# Usual initialization, ensure that the data is a piddle, and ensure that we
# have enough information for the bounds.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [$self->default_plot_type]
		unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Set the data and the bounders
	$self->_change_data($self->{data});
	
	# Make sure that we have bounds, or some combination of x_bounds, y_centers,
	# etc
	my @bounders = grep {
		/([xy]_)?bounds/
		or /[xy]_(centers|edges)/
	} keys %$self;
	
	# Nothing can be combined with 'bounds':
	croak("Cannot combine simple 'bounds' with more specific bounders")
		if @bounders > 1 and grep {$_ eq 'bounds'} @bounders;
	
	# Handle simple bounds
	if (@bounders == 1) {
		croak("Must specify bounds or a combination of other bounders")
			unless $bounders[0] eq 'bounds';
		
		$self->{x_bounds} = [$self->{bounds}->[0], $self->{bounds}->[2]];
		$self->{y_bounds} = [$self->{bounds}->[1], $self->{bounds}->[3]];
		
		delete $self->{bounds};
	}
	else {
		# make sure they specified bounds pairs:
		croak('Must specify one x and one y bounder')
			unless 1 == grep {/^x/} @bounders and 1 == grep {/^y/} @bounders;
	}

	# Handle the x-bounds
	if (grep {/^x_edges$/} @bounders) {
		$self->compute_centers('x');
	}
	elsif (grep {/^x_centers$/} @bounders) {
		$self->compute_edges('x');
	}
	elsif (grep {/^(x_)?bounds$/} @bounders) {
		my $x_edges = zeroes($self->get_data->dim(0) + 1)
			->xlinvals(@{$self->{x_bounds}});
		$self->edges('x', $x_edges);
	}
	# Handle the y-bounds
	if (grep {/^y_edges$/} @bounders) {
		$self->compute_centers('y');
	}
	elsif (grep {/^y_centers$/} @bounders) {
		$self->compute_edges('y');
	}
	elsif (grep {/^(y_)?bounds$/} @bounders) {
		my $y_edges = zeroes($self->get_data->dim(1) + 1)
			->xlinvals(@{$self->{y_bounds}});
		$self->edges('y', $y_edges);
	}
}


# Getter and setter
# Call like $x_edges = $dataset->edges('x');
# or $dataset->edges(x => $new_edges);
sub edges {
	my ($self, $axis) = (shift, shift);
	
	# Called as a setter
	if (@_) {
		$self->{$axis . '_edges'} = PDL::Core::topdl($_[0]);
		$self->compute_centers($axis);
		return;
	}
	
	# Called as a getter:
	return $self->{$axis . '_edges'};
}

# Getter and setter (see above for calling example)
sub centers {
	my ($self, $axis) = (shift, shift);
	
	# Called as a setter
	if (@_) {
		$self->{$axis . '_centers'} = PDL::Core::topdl($_[0]);
		$self->compute_edges($axis);
		return;
	}
	
	# Called as a getter:
	return $self->{$axis . '_centers'};
}

use PDL::NiceSlice;

sub compute_centers {
	my ($self, $axis) = @_;
	my $data = $self->{$axis . '_edges'};
	
	my ($type, $spacing) = $self->guess_scaling_for($data);
	my $results;
	if ($type eq 'linear') {
		$results = $data(0:-2) + $spacing / 2;
	}
	elsif ($type eq 'log') {
		$results = $data(0:-2) * sqrt($spacing);
	}
	else {
		croak ("Unknown scaling type $type; I only know about linear and log scaling");
	}
	
	$self->{$axis . '_centers'} = $results;
}

sub compute_edges {
	my ($self, $axis) = @_;
	my $data = $self->{$axis . '_centers'};
	
	my ($type, $spacing) = $self->guess_scaling_for($data);
	my @dims = $data->dims;
	$dims[0]++;
	my $results = zeroes($data->type, @dims);
	if ($type eq 'linear') {
		$results(0:-2) .= $data - $spacing / 2;
		$results(-1) .= $data(-1) + $spacing / 2;
	}
	elsif ($type eq 'log') {
		$results(0:-2) = $data / sqrt($spacing);
		$results(-1) .= $data(-1) * sqrt($spacing);
	}
	else {
		croak ("Unknown scaling type $type; I only know about linear and log scaling");
	}
	
	$self->{$axis . '_edges'} = $results;
}

=item get_data

Returns the piddle containing the data. 



=cut

sub get_data { return $_[0]->{data} }

=item change_data

Changes the data to the piddle passed in. For example,

 $map_plot->dataSets->{'intensity'}->change_data($new_intensity);

=cut

sub _change_data {
	my ($self, $data) = @_;
	
	# Ensure the data is a piddle:
	eval {
		$self->{data} = PDL::Core::topdl($data);
		1;
	} or croak('For Grid datasets, the data must be piddles '
		. 'or scalars that pdl() knows how to process');
}



=item guess_scaling_for

Takes a piddle and tries to guess the scaling from the spacing. Returns a string
indicating the scaling, either "linear" or "log", as well as the spacing term.

working here - clarify that last bit with an example

=cut

#	my $data	= exists $self->{"$axis-edges"}		? $self->{"$axis-edges"}
#				: exists $self->{"$axis-centers"}	? $self->{"$axis-edges"}
#				: die "Huh?";

sub guess_scaling_for {
	my ($self, $data) = @_;
	
	Carp::confess("data is not defined") if not defined $data;
	
	# One point is not allowed:
	croak("Cannot determine scaling with only one point!")
		if $data->dim(0) == 1;
	
	# Assume linear if there are only two points of data:
	return ('linear', $data(1) - $data(0)) if $data->dim(0) == 2;
	
	# Now we can really say something about linear vs logarithmic scaling:
	my $lin_spaces = $data(1:-1) - $data(0:-2);
	my $lin_space = $lin_spaces->average;
	my $lin_score = (($lin_spaces - $lin_space)**2)->sum / $lin_spaces->nelem;
	
	my $log_spaces = $data(1:-1) / $data(0:-2);
	my $log_space = $log_spaces->average;
	my $log_score = (($log_spaces - $log_space)**2)->sum / $log_spaces->nelem;
	
	return ('linear', $lin_space) if $lin_score < $log_score;
	return ('log', $log_space);
}

=back

=cut


#############################################################################
#                                   Image                                   #
#############################################################################

package PDL::Graphics::Prima::DataSet::Image;
use base 'PDL::Graphics::Prima::DataSet::Grid';
use Carp 'croak';

=head2 Image

Images are like Grids (they are derived from Grids, actually) but they have
a specified color format. Since they have a color format, this means that
they need to hold information for different aspects of each color, so they
typically have one more dimension than Grids. That is, where a grid might
have dimensions M x N, an rgb or hsv image would have dimensions 3 x M x N.

The default image format is rgb. Currently supported image formats are
rgb (red-green-blue), hsv (hugh-saturation-value), and prima (Prima's internal
color format, which is a packed form of rgb).

As Images are derived from Grids, any method you can call on a Grid you can
also call on an Image. Differences and details specific to Images include:

=over

=item ds::Image - short-name constructor

=for sig

    ds::Image($image, option => value, ...)

Creates an Image dataset. A particularly important key is the C<color_format>
key, which indicates the format of the C<$image> piddle. When it comes to
drawing the image, the data will be converted to a set of Prima colors,
which means that the first dimension will be reduced away. Values associated
with keys should be thread-compatible with the dimensions starting from the
second dimension, so if your image has dims 3 x M x N, values associated
with your various keys should be thread-compatible with an M x N piddle.

Note that color formats are case insensitive. At the moment there is no way
to add new color formats, but you should expect a color format API to come
at some point in the not-distant future. It will very likely make use of
L<PDL::Graphics::ColorSpace>, so if you want your own special color format
to be used for Images, you should contribute to that project.

=cut

sub ds::Image {
	croak('ds::Image expects data and then key => value pairs, but you'
		. ' supplied an even number of arguments') if @_ % 2 == 0;
	my $data = shift;
	return PDL::Graphics::Prima::DataSet::Image->new(@_, data => $data);
}


# A color conversion lookup table. This converts any of the specified
# formats into a Prima color piddle. At some point this needs to be
# extracted into something more general.
my %color_convert_func_for = (
	prima => sub { return $_[0]->mv(-1,0)->clump(2) },
	rgb => sub { return $_[0]->mv(-1,0)->rgb_to_color },
	hsv => sub { return $_[0]->mv(-1,0)->hsv_to_rgb->rgb_to_color },
);

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Image'}
sub default_plot_type { pimage::Basic() }

=item change_data

Sets the image to the new image data. Expects a piddle with the new data
and an optional format specification. If no specification is given, the
current format is used.

=cut

sub _change_data {
	my ($self, $image, $format) = @_;
	$format = $self->{color_format} unless defined $format;
	
	$format = lc $format;
	# Make sure a valid color format is given:
	croak("'$format' is not a known color format")
		unless exists $color_convert_func_for{$format};
	$self->{color_format} = $format;
	
	# Ensure the data is a piddle:
	eval {
		$self->{data} = PDL::Core::topdl($image);
		1;
	} or croak('For Image datasets, the data must be piddles '
		. 'or scalars that pdl() knows how to process');
	
	# Move the color dims to the back so Grid operations work without
	# modifiction. Note that PrimaColors is a special case that does not need
	# a first dimension in the way that the other formats do. To make it work
	# just like the other formats, add a dummy dimension of size 1 if no
	# first dimension exists.
	$self->{data} = $self->{data}->dummy(0)
		if $self->{color_format} and $self->{color_format} eq 'prima'
				and $self->{data}->dim(0) ne 1;
	$self->{data} = $self->{data}->mv(0,-1);
}

sub init {
	my $self = shift;
	
	# Assume RGB if no color format is given:
	$self->{color_format} ||= 'rgb';
	
	# Initialize the parent class:
	$self->SUPER::init;
}

# Converts the current data into a Prima color format.
use PDL::Drawing::Prima::Utils;
no PDL::NiceSlice;
sub get_prima_color_data {
	my $self = shift;
	return $color_convert_func_for{$self->{color_format}}->($self->get_data);
}

##############################################################################
#                                    Func                                    #
##############################################################################

=head2 Func

PDL::Graphics::Prima provides a special pair dataset that takes a function
reference instead of a set of data. The function should take a piddle of x-values
as input and compute and return the y-values. You can specify the number of data
points by supplying

 N_points => value

in the list of key-value pairs that initialize the dataset. Most of the
functionality is inherited from C<PDL::Graphics::Prima::DataSet::Pair>, but
there are a few exceptions.

=cut

package PDL::Graphics::Prima::DataSet::Func;
our @ISA = qw(PDL::Graphics::Prima::DataSet::Pair);

use Carp 'croak';

=over

=item ds::Func - short-name constructor

=for sig

    ds::Func($subroutine, option => value, ...)

The short-name constructor to create function datasets. The subroutine must be
a reference to a subroutine, or an anonymous sub. For example, 

=for example

 # Reference to a subroutine,
 # PDL's exponential function:
 ds::Func (\&PDL::exp)
 
 # Using an anonymous subroutine:
 ds::Func ( sub {
     my $xs = shift;
     return $xs->exp;
 })


=cut

sub ds::Func {
	croak('ds::Func expects a function reference and then key => value pairs, but you'
		. ' supplied an even number of arguments') if @_ % 2 == 0;
	return PDL::Graphics::Prima::DataSet::Func->new(func => @_);
}

# Even less to do for this than for a normal dataset. Just verify the function,
# store it in $self, and ensure we have a sensible value for N_points. Note
# that there is no way to ensure that the supplied function takes a piddle;
# we'll just have to take it on faith.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [ppair::Lines] unless exists $self->{plotTypes};
	
	# Set the default number of data points (for evaluated data) to 200:
	$self->{N_points} ||= 200;
	
	# Ensure that they provided *something* for the func key
	croak('You must supply a function under the "func" key')
		unless exists $self->{func};
	
	# Set the function and number of points
	$self->_change_data($self->{func}, $self->{N_points});
}

=item change_data

Sets the function and/or the number of points to evaluate. The basic usage
looks like this:

 $plot->dataSets->{'curve'}->change_data(\&some_func, $N_points);

Either of the arguments can be undefined if you want to change only the
other. That means that you can change the function without changing the
number of evaluation points like this:

 $plot->dataSets->{'curve'}->change_data(\&some_func);

and you can change the number of evaluation points without changing the
function like this:

 $plot->dataSets->{'curve'}->change_data(undef, $N_points);

=cut

sub _change_data {
	my ($self, $func_ref, $N_points) = @_;
	
	if (defined $func_ref) {
		croak('Func datasets require a function reference, but what you '
			. 'supplied is not a function reference')
			unless ref($func_ref) and ref($func_ref) eq 'CODE';
		
		$self->{func} = $func_ref;
	}
	
	if (defined $N_points) {
		croak("N_points must be a positive number")
			unless $N_points =~ /^\d+$/ and $N_points > 0;
		
		$self->{N_points} = $N_points;
	}
}

=item get_xs, get_ys

These functions override the default Pair behavior by generating the x-data
and using that to compute the y-data. The x-data is uniformly sampled
according to the x-axis scaling.

=cut

sub get_xs {
	my ($self) = @_;
	my $x_axis = $self->widget->x;
	
	# working here - implement caching as an option in $self, like this:
	#if ($self->{cacheData}) ...
	return $x_axis->{scaling}->sample_evenly($x_axis->minmax, $self->{N_points});
}
sub get_ys {
	my ($self) = @_;
	my $xs = $self->get_xs;
	return $self->{func}->($xs);
}

=item compute_collated_min_max_for

This function is supposed to provide information for autoscaling. This is a
sensible thing to do for the the y-values of functions, but it makes no
situation with the x-values since these are taken from the x-axis min and
max already.

This could be smarter, methinks, so please give me your ideas if you have
them. :-)

=cut

# Function-based datasets need to return a collection of bad values for x-axis
# requirements.
sub compute_collated_min_max_for {
	# Must get the collated min max for each plot type for this data:
	my ($self, $axis_name, $pixel_extent) = @_;
	
	if ($axis_name eq 'x') {
		return (
			PDL->zeroes($pixel_extent+1)->setvaltobad(0),
			PDL->zeroes($pixel_extent+1)->setvaltobad(0),
		);
	}
	
	# working here - this needs to be smarter, especially for the first round
	# when *nothing* is known about the axis limits.
	
	my $widget = $self->{dataSets}->{widget};
	
	my (@min_collection, @max_collection);
	foreach my $plotType (@{$self->{plotTypes}}) {
		
		# Accumulate all the collated results
		my ($min, $max)
		= $plotType->compute_collated_min_max_for($axis_name, $pixel_extent);
		# The collated results are not required to be one dimensional.
		# As such, I need to reduce them. I do this by moving the dimension
		# with $pixel_extent entries to the back and then calling minimum
		# until I have only one dimension remaining.
		$min = $min->squeeze->mv(0,-1);
		$min = $min->minimum while($min->ndims > 1);
		$max = $max->squeeze->mv(0,-1);
		$max = $max->maximum while($max->ndims > 1);
		push @min_collection, $min;
		push @max_collection, $max;
	}
	
	# Merge all the data:
	my $collated_min = PDL::cat(@min_collection)->mv(-1,0)->minimum;
	my $collated_max = PDL::cat(@max_collection)->mv(-1,0)->maximum;
	
	return ($collated_min, $collated_max);
}

=back

=cut

=head2 Annotation

PDL::Graphics::Prima provides a generic annotation dataset that is used for
adding various annotations to your plots. It expects a list of key/value pairs
where the keys are the names you give to your annotations and the values are
annotation plotType objects.

=cut

###############################################################################
#                                  Annotation                                  #
###############################################################################

package PDL::Graphics::Prima::DataSet::Annotation;
our @ISA = qw(PDL::Graphics::Prima::DataSet);

use Carp 'croak';

=over

=item ds::Note - short-name constructor for annotations

=for sig

    ds::Note(plotType, plotType, ...)

The short-name constructor to create annotations. For example, 

=for example

 ds::Note(
     pnote::Region(
         # args here
     ),
     ...
 );

=cut

sub ds::Note {
	return PDL::Graphics::Prima::DataSet::Annotation->new(plotTypes => [@_]);
}

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Annotation'}



sub init {
	my $self = shift;
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
}

sub change_data {
	carp("Annotations do not have data that can be changed; ignoring");
}

=back

=cut

#############################################################################
#                            Dataset::Collection                            #
#############################################################################

=head1 DataSet::Collection

The dataset collection is the thing that actually holds the datasets in the
plot widget object. The Collection is a tied hash, so you access all of its
data members as if they were hash elements. However, it does some
double-checking for you behind the scenes to make sure that whenever you
add a dataset to the Collection, that you added a real DataSet object and
not some arbitrary thing.

working here - this needs to be clarified

=cut

package PDL::Graphics::Prima::DataSet::Collection;
# This package implements the tied array functionality needed for automatic
# data validation for the array setting operations.

use Tie::Hash;
our @ISA = qw(Tie::StdHash);

# Validate and convert the data, and set the graph's min/max values if they are
# on auto mode.
use Carp 'croak';

sub TIEHASH {
	my ($class, $widget) = @_;
	my $self = {widget => $widget};
	return bless $self, $class;
}

# Override delete to prevent removing the widget key:
sub DELETE {
	my ($this, $key) = @_;
	return if $key eq 'widget';
	delete $this->{$key};
	$this->{widget}->notify('ChangeData');
}

# Clear the contents of the hash while preserving the widget:
sub CLEAR {
	my ($this) = @_;
	%{$this} = (widget => $this->{widget});
	$this->{widget}->notify('ChangeData');
}

sub STORE {
	my ($self, $name, $dataset) = @_;
	# Silently do nothing if they try to change the widget:
	return if $name eq 'widget';
	
	# They must pass in a DataSet object:
	croak('You can only add dataSet objects to the collection of dataSets')
		unless eval {$dataset->isa('PDL::Graphics::Prima::DataSet')};
	
	# Store it:
	$self->{$name} = $dataset;
	
	# Inform the dataset of its parent widget:
	$dataset->widget($self->{widget});

	# Call data initialization for all of the plot types:
	# working here - still do this? I think that histograms will want this,
	# maybe.
#	$dataset->initialize_plot_types;
	
	# Recompute the auto min/max values:
	$self->{widget}->notify('ChangeData');
}




1;

=head1 RESPONSIBILITIES

The datasets and the dataset collection have a number of responsibilities, and
a number of things for whch they are not responsible. 

The dataset container is responsible for:

=over

=item knowing the plot widget

The container always maintains knowledge of the plot widget to which it belongs.
Put a bit differently, a dataset container cannot belong to multiple plot
widgets (at least, not at the moment).

=item informing datasets of their container and plot widget

When a dataset is added to a dataset collection, the collection is responsible
for informing the dataset of the plot object and the dataset collection to which
the dataset belongs.

=back

Datasets themselves are responsible for:

=over

=item knowing and managing the plotTypes

The datasets are responsible for maintaining the list of plotTypes that are to
be applied to their data.

=item knowing per-dataset properties

Drawing properties can be specified on a per-dataset scope. The dataset is
responsible for maintaining a list of these properties and providing them to
the plot types when they perform drawing operations.

=item knowing the dataset container and the plot widget

All datasets know the dataset container and the plot widget to which they belong.
Although they could retrieve the widget through a method on the container, the


=item informing plotTyes' plot widget

The plot types all know the widget (and dataset) to which they belong, and it is
the 

=item managing the drawing operations of plotTypes

Although datasets themselves do not need to draw anything, they do call the
drawing operations of the different plot types that they contain. 

=item knowing and supplying the data

A key responsibility for the dataSets is holding the data that are drawn by the
plot types. Althrough the plot types may hold specialized data, the dataset
holds the actual data the underlies the plot types and provides a specific
interface for the plot types to access that data.

=back

On the other hand, datasets are not responsible for knowing or doing any of the
following:

=over

=item knowing axes

The plot object is responsible for knowing the x- and y-axis objects. However,
if the axis system is changed to allow for multiple x- and y-axes, then this
burden will shift to the dataset as it will need to know which axis to use when
performing data <-> pixel conversions.

=back

=head1 TODO

Add optional bounds to function-based DataSets.

Captitalization for plotType, etc.

Use PDL documentation conventions for signatures, ref, etc.

Additional datset, a two-tone grid. Imagine that you want to overlay the
population density of a country and the average rainfall (at the granularity
of counties, let's say). You could use the intensity of the red channel to
indicate population and the intensity of blue to indicate rainfall. Highly
populated areas with low rainfall would be bright red, while highly populated
areas with high rainfall would be purple, and low populated areas with high
rainfall would be blue. The color scale would be indicated
with a square with a color gradient (rather than a horizontal or vertical bar
with a color gradient, as in a normal ColorGrid). Anyway, this differs from
a normal grid dataset because it would require two datasets, one for each
tone.

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

=item L<PDL::Graphics::Prima::ReadLine>

Encapsulates all interaction with the L<Term::ReadLine> family of
modules.

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
