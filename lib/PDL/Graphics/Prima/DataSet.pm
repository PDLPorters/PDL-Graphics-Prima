use strict;
use warnings;

# Codifies the different kinds of dataset plotting that you can do, and defines
# the class for the tied dataset array.

=head1 NAME

PDL::Graphics::Prima::DataSet - the way we think about data

=head1 SYNOPSIS

 -distribution => ds::Set(
     $data, plotType => pset::CDF
 ),
 -lines => ds::Seq(
     $x, $y, plotType => [ppair::Lines, ppair::Diamonds]
 ),
 -contour => ds::Grid(
     $matrix, bounds => [$left, $bottom, $right, $top],
              y_edges => $ys, x_bounds => [$left, $right],
              x_edges => $xs, y_bounds => [$bottom, $top],
              plotType => pgrid::Color(palette => $palette),
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
heights of a class of students. A Pair collection is an ordered collection of x/y
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

=item widget

The widget associated with the dataset.

=cut

sub widget {
	# Simply return the widget if it's a getter:
	return $_[0]->{widget} if @_ == 1;
	
	# It's a setter call, so set self's and all plotTypes' widget
	my ($self, $widget) = @_;
	$self->{widget} = $widget;
	
	# Set the widget for all plotTypes
	my $plotTypes = $self->{plotTypes};
	foreach (@$plotTypes) {
		$_->widget($widget);
	}
}

=item draw

Calls all of the drawing functions for each plotType of the dataset. This also
applies all the global drawing options (like C<color>, for example) that were
supplied to the dataset.

=cut

# Calls all the drawing functions for the plotTypes for this dataset:
sub draw {
	my ($dataset) = @_;
	my $widget = $dataset->widget;
	
	my @drawing_parameters = qw(color backColor linePattern lineWidth lineJoin
			lineEnd rop rop2);
	
	# backup the dataset-wide drawing parameters:
	my %backups;
	foreach(@drawing_parameters) {
		if (exists $dataset->{$_}) {
			$backups{$_} = $dataset->{$_};
		}
		elsif (not exists $dataset->{$_.'s'}) {
			$backups{$_} = $widget->$_;
		}
	}

	# Call each plot type's drawing function, in the order specified:
	foreach my $plotType (@{$dataset->{plotTypes}}) {
		# set the default drawing parameters and draw the plot type
		$widget->set(%backups);
		$plotType->draw;
	}
	$widget->set(%backups);
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

This is the unversal constructor that is called by the short-name constructors
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
	
	$self->init;
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
			. ref($_)) unless eval{$_->isa($expected_plot_class)};
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

=back

=cut

################################################################################
#                                     Sets                                     #
################################################################################

package PDL::Graphics::Prima::DataSet::Set;
use base 'PDL::Graphics::Prima::DataSet';
use Carp;
use strict;
use warnings;

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
	$self->{plotTypes} = [pset::CDF] unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Ensure the data is a piddle:
	eval {
		$self->{data} = PDL::Core::topdl($self->{data});
	};
	return unless $@;
	
	croak('For Set datasets, the data must be piddles '
		. 'or scalars that pdl() knows how to process');
	
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


=back

=cut

###############################################################################
#                                    Pairs                                    #
###############################################################################

package PDL::Graphics::Prima::DataSet::Pairs;
use base 'PDL::Graphics::Prima::DataSet';
use Carp 'croak';
use strict;
use warnings;

=head2 Pairs

Pairs are collections of paired data. A typical set of pairs is the sort of
thing you would visualize with an x/y plot: a time series
such as the series of high temperatures for each day in a month or the x- and
y-coordinates of a bug walking across your desk. PDL::Graphics::Prima provides
many ways of visualizing Pairs, as discussed under
L<PDL::Graphics::Prima::PlotType/Pairs>.

WORKING HERE - change things over from Sequence to Pairs

The dimensions of pluralized properties (i.e. C<colors>) should
thread-match the dimensions of the data. An important exception to this is
C<ppair::Lines>, in which case you must specify how you want properties to thread.

The default plot type is C<ppair::Diamonds>.

=over

=item ds::Pairs - short-name constructor

=for sig

    ds::Pairs($x_data, $y_data, option => value, ...)

The short-name constructor to create sequences. The data can be either a piddle
of values or an array reference of values (which will be converted to a piddle
during initialization).

=cut

sub ds::Pairs {
	croak('ds::Pairs expects x- and y-data and then key => value pairs, but you'
		. ' supplied an odd number of arguments') if @_ % 2 == 1;
	my ($x_data, $y_data) = (shift, shift);
	return PDL::Graphics::Prima::DataSet::Pairs->new(@_
		, x => $data, y => $y_data);
}

=item expected_plot_class

Sequences expect plot type objects that are derived from
C<PDL::Graphics::Prima::PlotType::Pairs>.

=cut

sub expected_plot_class {'PDL::Graphics::Prima::PlotType::Pairs'}

# Standard initialization, and ensures that the data are piddles.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [ppair::Diamonds] unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Ensure the data are piddles:
	eval {
		$self->{x} = PDL::Core::topdl($self->{x});
		$self->{y} = PDL::Core::topdl($self->{y});
	};
	return unless $@;
	
	croak('For Sequence datasets, the x- and y-data must be piddles '
		. 'or scalars that pdl() knows how to process');
}

=item get_xs, get_ys, get_data

Returns piddles with the x, y, or x-y data. The last function returns two
piddles in a list.

=cut

sub get_xs { $_[0]->{xs} }
sub get_ys { $_[0]->{ys} }
sub get_data {
	return ($_[0]->{xs}, $_[0]->{ys});
}

=item get_data_as_pixels

Uses the reals_to_pixels functions for the x- and y- axes to convert the
values of the x- and y- data to actual pixel positions in the widget.

=cut

sub get_data_as_pixels {
	my ($dataset) = @_;
	my $widget = $dataset->widget;
	
	my ($xs, $ys) = $dataset->get_data;
	return ($widget->x->reals_to_pixels($xs), $widget->y->reals_to_pixels($ys));
}

=back

=cut

# working here - create an OrderedSet, which only requires a single set of
# data and uses counting numbers for the "x" values

###############################################################################
#                                    Grids                                    #
###############################################################################

package PDL::Graphics::Prima::DataSet::Grid;
use base 'PDL::Graphics::Prima::DataSet';
use Carp 'croak';
use strict;
use warnings;

=head2 Grids

Grids are collections of data that is regularly ordered in two dimensions. Put
differently, it is a structure in which the data is described by two indices.
The analogous mathematical structure is a matrix and the analogous visual is an
image. PDL::Graphics::Prima provides a few ways to visualize grids, as
discussed under L<PDL::Graphics::Prima::PlotType/Grids>. The default plot type
is C<pseq::Color>.

This is the least well thought-out dataSet. As such, it may change in the
future. All such changes will, hopefully, be backwards compatible.

At the moment, there is only one way to visualize grid data: C<pseq::Color>.
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
C<y_edges would have 4 elements:

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

# Usual initialization, ensure that the data is a piddle, and ensure that we
# have enough information for the bounds.
sub init {
	my $self = shift;
	
	# Supply a default plot type:
	$self->{plotTypes} = [pgrid::Color] unless exists $self->{plotTypes};
	
	# Check that the plotTypes are valid:
	$self->check_plot_types(@{$self->{plotTypes}});
	
	# Ensure the data is a piddle:
	eval {
		$self->{data} = PDL::Core::topdl($self->{data});
		1;
	} or croak('For Grid datasets, the data must be piddles '
		. 'or scalars that pdl() knows how to process');
	
	# Make sure that we have bounds, or some combination of x_bounds, y_centers,
	# etc
	my @bounders = grep {
		/([xy]_)?bounds/
		or /[xy]_(centers|edges)/
	} keys %$self;
	
	# Nothing can be combined with 'bounds':
	croak("Cannot combined simple 'bounds' with more specific bounders")
		if @bounders > 1 and grep {$_ eq 'bounds'} @bounders;
	
	# Handle simple bounds
	if (@bounders == 1) {
		croak("Must specify bounds or a combination of other bounders")
			unless $bounders[0] eq 'bounds';
		
		# working here
	}
	# Handle bounds pairs:
	else {
		croak('Must specify one x and one y bounder')
			unless 1 == grep {/^x/} @bounders and 1 == grep {/^y/} @bounders;
		
		# working here
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
		$self->compute_edges('x');
		return;
	}
	
	# Called as a getter:
	return $self->{$axis . '_centers'};
}

sub compute_centers {
	my ($self, $axis) = @_;
	my $data = $self->{$axis . '_edges'};
	
	my ($type, $spacing) = $self->guess_scaling_for($data);
	my $results;
	if ($type eq 'linear') {
		$results = $data(0:-2) + $spacing / 2;
	}
	else {
		$results = $data(0:-2) * sqrt($spacing);
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
	else {
		$results(0:-2) = $data / sqrt($spacing);
		$results(-1) .= $data(-1) * sqrt($spacing);
	}
	
	$self->{$axis . '_edges'} = $results;
}

=item get_data

Returns the piddle containing the data. 



=cut

sub get_data {
	return $_[0]>{data};
}


=item guess_scaling_for

Takes a piddle and tries to guess the scaling from the spacing. Returns a string
indicating the scaling, either "linear" or "log", as well as the spacing term.

working here - clarify that last bit with an example

=cut

#	my $data	= exists $self->{"$axis-edges"}		? $self->{"$axis-edges"}
#				: exists $self->{"$axis-centers"}	? $self->{"$axis-edges"}
#				: die "Huh?";

use PDL::NiceSlice;

sub guess_scaling_for {
	my ($self, $data) = @_;
	
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


###############################################################################
#                                    Other                                    #
###############################################################################

=head2 PDL::Graphics::Prima::DataSet::Func

This dataset only takes a function reference and computes the values it needs
from the min/max bounds of the plotting. You can specify the number of data
points by supplying

 N_points => value

in the list of key-value pairs that initialize the dataset.

working here - clarify and expand

=cut

package PDL::Graphics::Prima::DataSet::Func;
our @ISA = qw(PDL::Graphics::Prima::DataSet);

use Carp 'croak';
use strict;
use warnings;

# Even less to do for this than for a normal dataset. Just store the function in
# $self and ensure we have a sensible value for N_points:
sub initialize {
	my ($dataset, $array_ref) = @_;
	$dataset->{func} = $array_ref->[0];
	
	# Set the default number of data points (for evaluated data) to 200:
	$dataset->{N_points} ||= 200;
	croak("N_points must be a positive number")
		unless $dataset->{N_points} =~ /^\d+$/ and $dataset->{N_points} > 0;
}

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

sub get_data {
	my ($dataset) = @_;
	
	my $xs = $dataset->get_xs;
	return ($xs, $dataset->{func}->($xs));
}

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


=head2 DataSet::Collection

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
use parent -norequire, 'Tie::StdHash';

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
}

# Clear the contents of the hash while preserving the widget:
sub CLEAR {
	my ($this) = @_;
	%{$this} = (widget => $this->{widget}); 
}

sub STORE {
	my ($self, $name, $dataset) = @_;
	# Silently do nothing if they try to change the widget:
	return if $name eq 'widget';
	
	# They must pass in a DataSet object:
	croak('You can only add dataSet objects to the collection of dataSets')
		unless eval {$dataset->isa('PDL::Graphics::Prima::DataSet')});
	
	# Store it:
	$self->{$name} = $dataset;
	
	# Inform the dataset of its parent widget:
	$dataset->widget($self->{widget});

	# Call data initialization for all of the plot types:
	# working here - still do this? I think that histograms will want this,
	# maybe.
#	$dataset->initialize_plot_types;
	
	# Recompute the auto min/max values:
	$self->{widget}->x->update_edges;
	$self->{widget}->y->update_edges;
}




1;

=head1 TODO

Add optional bounds to function-based DataSets.

PairSet: an unordered collection of pairs of data, like collections of
individuals' height and weight. These data would be binned in two dimensions
and the binning counts would be visualized with a grid.

Captitalization for plotType, etc.

Use PDL documentation conventions for signatures, ref, etc.

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
