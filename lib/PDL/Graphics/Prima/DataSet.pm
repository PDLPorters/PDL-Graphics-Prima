use strict;
use warnings;

# Codifies the different kinds of dataset plotting that you can do, and defines
# the class for the tied dataset array.

=head1 NAME

PDL::Graphics::Prima::DataSet - the way we think about data

=head1 SYNOPSIS

 # working here

=head1 DESCRIPTION

C<PDL::Graphics::Prima> differentiates between a few kinds of data: Sets,
Sequences, and Grids. A Set is an unordered collection of data, such as the
heights of a class of students. A Sequence is an ordered collection of x/y
pairs, such as a time series. A Grid is, well, a matrix, such as the pixel
colors of a photograph.

=head2 Sets

# working here

=pod

At the moment there are two kinds of datasets. The piddle-based datasets have
piddles for their x- and y-data. The function-based datasets create their
x-values on the fly and evaluate their y-values using the supplied function
reference. The x-values are generated using the C<sample_evenly> function which
belongs to the x-axis's scaling object/class. As such, any scaling class needs
to implement a C<sample_evenly> function to support function-based datasets.

=cut


package PDL::Graphics::Prima::DataSet;

use PDL::Graphics::Prima::PlotType;
use PDL::Core ':Internal'; # for topdl
use Carp 'croak';
use strict;
use warnings;

=head2 PDL::Graphics::Prima::DataSet::new

This creates a new dataset. It is written to handle subclasses correctly, and
as such should not generally be overridden. Override the C<initialize> function
instead.

There are at least three ways to indicate the x/y coordinates for a dataset.

=over

=item explicit x, y

You can provide explicit x- and y- piddles for your data.

=item explicit x, function for y

You can provide a set of x values and a function that the plotter evaluates
for you at those x values.

=item no x, function for y

You can simply provide a function for y which is evaluated based upon the bounds
of the plot. This can lead to problems if you simply try to plot a function without
specifying those bounds. It will try to compute the bounds automatically and
then fail.

=back

The new function takes a class name and an anonymous array. The anonymous array
has either one or two x/y arguments (as just explained) followed by key => value
pairs. All key => value pairs are stored in the underlying hash that constitutes
the DataSet object.

The keys associated with arguments to the polylines command are used during the
drawing commands. In particular, color, backColor, linePattern, lineWidth,
lineJoin, lineEnd, rop, and rop2 can be used to specify each of these properties
for the drawing of the function. Furthermore, their plurals can be used to
specify piddles for these properties if you want to PDL-thread different
properties for different curves. That is particularly useful if you are drawing
(say) 30 lines for which the line color indicates a parameter that describes the
line.

For example, this draws a red curve:

  [ $x, $y, color => cl::Red ]

This draws ideally multiple curves in which the curve colors are specified in
C<$colors_piddle>:

  [ $xs, $ys, colors => $colors_piddle ]

=for details
See the drawing function.

=cut

# Takes an array ref and creates a new dataset object. This works for both the
# base class and for derived classes. To change behavior at creation time,
# derived classes should override the initialize function.
sub new {
	my ($class, $array_ref, $widget) = @_;
	my @array = @$array_ref;
	
	# pull out the x/y data and initialize self to the remaining elements of
	# the passed array ref. The array ref either contains a code ref and
	# then key/value pairs, or the x and y data, followed by key/value pairs:
	my @args;
	if (ref($array[1]) eq 'CODE') {
		@args = @array[2..$#array];
	}
	elsif (ref($array[0]) eq 'CODE') {
		@args = @array[1..$#array];
	}
	else {
		@args = @array[2..$#array];
	}
	croak("Arguments must be passed as key/value pairs")
		unless @args % 2 == 0;
	my $self = {@args, widget => $widget};
	
	# Make sure self has a plotType option:
	$self->{plotType} = pt::Lines unless exists $self->{plotType};
	
	# Make sure the plotType option is packed into an anonymous array:
	$self->{plotType} = [$self->{plotType}]
		unless ref($self->{plotType}) eq 'ARRAY';
	
	# Bless the hash into the class and tell all the plot types the dataset
	# and widget to which they belong:
	bless ($self, $class);
	foreach (@{$self->{plotType}}) {
		$_->widget($widget);
		$_->dataset($self);
	}
	
	# call the class's initialization function:
	$self->initialize($array_ref);
	
	return $self;
}

=head2 initialize

Perform any dataset-specific initialization. For piddle-based datasets, this
simply ensures that the data stored in the object are piddles. In other words,
if you pass in a scalar value, it will be converted to a piddle internally, to
simplify the internal routines.

Note that part of the initialization (at the moment) is to set infinite and nan
values to bad in place, which modifies the underlying piddle.

In the base class, the initialize function converst scalars to piddles. These
can be constants, or they can be array refs, since the pdl function knows how to
convert array refs to piddles.

=cut

sub initialize {
	# Not much to do here. This just pulls the x- and y- data out of the passed
	# array reference, packages them in piddles if they are scalars, and stores
	# them in $self:
	my ($dataset, $array_ref) = @_;
	my $xs = $array_ref->[0];
	my $ys = $array_ref->[1];
	eval {
		$xs = topdl($xs);
		$ys = topdl($ys);
# Unnecessary with the collation setup:
#		# Set infinity and nan to bad values:
#		$xs->inplace->setnantobad;
#		$ys->inplace->setnantobad;
		$dataset->{xs} = $xs;
		$dataset->{ys} = $ys;
	};
	return unless $@;
	
	croak('For standard datasets, the arguments must be piddles '
		. 'or scalars that pdl() knows how to process');
}

=head2 widget

The widget associated with the dataset.

=cut

sub widget {
	$_[0]->{widget} = $_[1] if @_ == 2;
	return $_[0]->{widget};
}


=head2 draw

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
	foreach my $plotType (@{$dataset->{plotType}}) {
		# set the default drawing parameters and draw the plot type
		$widget->set(%backups);
		$plotType->draw;
	}
	$widget->set(%backups);
}

# Returns the data values from the dataset. The widget is required for the
# function-based datasets:

=head2 get_xs, get_ys, get_data

Returns piddles with the x, y, or x-y data. The last function returns two
piddles.

=cut

sub get_xs { $_[0]->{xs} }
sub get_ys { $_[0]->{ys} }
sub get_data {
	return ($_[0]->{xs}, $_[0]->{ys});
}

=head2 get_data_as_pixels

Uses the reals_to_pixels functions for the x- and y- axes to convert the
values of the x- and y- data to actual pixel positions in the widget.

=cut

sub get_data_as_pixels {
	my ($dataset) = @_;
	my $widget = $dataset->widget;
	
	my ($xs, $ys) = $dataset->get_data;
	return ($widget->x->reals_to_pixels($xs), $widget->y->reals_to_pixels($ys));
}

=head2 compute_collated_min_max_for

working here

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
	foreach my $plotType (@{$self->{plotType}}) {
		
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

# working here - implement FuncBoth

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
	foreach my $plotType (@{$self->{plotType}}) {
		
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
	my ($self, $key, $value) = @_;
	# The value needs to be an anonymous array with arguments suitable for
	# data sets. Optional arguments are passed in hash references, and one of
	# the options is a plotType key. That is actually a class name that is used
	# to turn the supplied anonymous array into an object (after validating the
	# data, of course).
	croak('You can only add anonymous arrays or dataSet objects to dataSets')
		unless ref($value) and (ref($value) eq 'ARRAY'
					or eval {$value->isa('PDL::Graphics::Prima::DataSet')});
	
	# Silently do nothing if they try to change the widget:
	return if $key eq 'widget';
	
	# Create a dataset if it's not a blessed object:
	my $dataset;
	if (ref($value) eq 'ARRAY') {
		# If the first argument is a code reference, then the user simply wants
		# to plot a function.
		if (ref($value->[0]) and ref($value->[0]) eq 'CODE') {
			# If the second argument is *also* a code reference, then they
			# want a two-function:
			if (ref($value->[1]) and ref($value->[1]) eq 'CODE') {
				$dataset = PDL::Graphics::Prima::DataSet::FuncBoth->new($value, $self->{widget});
			}
			else {
				$dataset = PDL::Graphics::Prima::DataSet::Func->new($value, $self->{widget});
			}
		}
		# Otherwise, the user must specify both the x- and y- data sets.
		else {
			$dataset = PDL::Graphics::Prima::DataSet->new($value, $self->{widget});
		}
	}
	else {
		$dataset = $value;
	}
	
	# Store it:
	$self->{$key} = $dataset;

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
