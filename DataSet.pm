use strict;
use warnings;

# Codifies the different kinds of dataset plotting that you can do, and defines
# the class for the tied dataset array.

package Prima::Ex::Graph::DataSetArray;
# This package implements the tied array functionality needed for automatic
# data validation for the array setting operations.

use base 'Tie::Array';

# Required. Stores the widget where I can retrieve it:
sub TIEARRAY {
	my ($class, $widget) = @_;
	my $self = {widget => $widget, N_elem => 0};
	return bless $self, $class;
}

# Fetch simply uses the index as a hash key:
sub FETCH {
	my ($self, $index) = @_;
	return $self->{$index};
}

# Returns N_elem:
sub FETCHSIZE {
	return $_[0]->{N_elem};
}

# Sets N_elem and removes unwanted entries:
sub STORESIZE {
	my ($self, $N_elem) = @_;
	# Remove unwanted entries:
	if ($N_elem < $self->{N_elem}) {
		for ($N_elem .. $self->{N_elem}-1) {
			delete $self->{$_};
		}
	}
	# Store the final number:
	$self->{N_elem} = $N_elem;
}

# Does nothing:
sub EXTEND {}

sub EXISTS {
	my ($self, $index) = @_;
	return exists $self->{$index};
}

sub DELETE {
	my ($self, $index) = @_;
	delete $self->{$index};
}

# Validate and convert the data, and set the graph's min/max values if they are
# on auto mode.
use Carp 'croak';
sub STORE {
	my ($self, $index, $value) = @_;
	# The value needs to be an anonymous array with arguments suitable for
	# data sets. Optional arguments are passed in hash references, and one of
	# the options is a plotType key. That is actually a class name that is used
	# to turn the supplied anonymous array into an object (after validating the
	# data, of course).
	croak('You can only add anonymous arrays or dataSet objects to dataSets')
		unless ref($value) and (UNIVERSAL::isa($value, 'ARRAY')
						or UNIVERSAL::isa($value, 'Prima::Ex::Graph::DataSet'));
	
	# Create a dataset if it's not a blessed object:
	my $dataset;
	if (ref($value) eq 'ARRAY') {
		if (ref($value->[0]) and ref($value->[0]) eq 'CODE') {
			$dataset = Prima::Ex::Graph::DataSet::Func->new($value);
		}
		else {
			$dataset = Prima::Ex::Graph::DataSet->new($value);
		}
	}
	else {
		$dataset = $value;
	}
	
	$self->{N_elem} = $index+1 if ($self->{N_elem} <= $index);
	
	# Store it:
	$self->{$index} = $dataset;

	# Call data initialization for all of the plot types:
	# working here - still do this? I think that histograms will want this,
	# maybe.
#	$dataset->initialize_plot_types;
	
	# Recompute the auto min/max values:
	$self->{widget}->x->recompute_auto;
	$self->{widget}->y->recompute_auto;
}

=pod

At the moment there are two kinds of datasets. The piddle-based datasets have
piddles for their x- and y-data. The function-based datasets create their
x-values on the fly and evaluate their y-values using the supplied function
reference. The x-values are generated using the C<sample_evenly> function which
belongs to the x-axis's scaling object/class. As such, any scaling class needs
to implement a C<sample_evenly> function to support function-based datasets.

=cut


package Prima::Ex::Graph::DataSet;

use Prima::Ex::Graph::PlotType;

use Carp 'croak';

=head2 Prima::Ex::Graph::DataSet::new

This creates a new dataset. It is written to handle subclasses correctly, and
as such should not generally be overridden. Override the C<initialize> function
instead.

=cut

# Takes an array ref and creates a new dataset object. This works for both the
# base class and for derived classes.
sub new {
	my ($class, $array_ref) = @_;
	my @array = @$array_ref;
	
	# initialize self to the passed hash, or an empty one if none was supplied.
	# The array ref either contains a code ref and then key/value pairs, or
	# the x and y data, followed by key/value pairs:
	my @args;
	if (ref($array[0]) eq 'CODE') {
		@args = @array[1..$#array];
	}
	else {
		@args = @array[2..$#array];
	}
	croak("Arguments must be passed as key/value pairs")
		unless @args % 2 == 0;
	my $self = {@args};
	
	# Make sure self has a plotType option:
	$self->{plotType} = pt::Lines unless exists $self->{plotType};
	
	# Make sure the plotType option is packed into an anonymous array:
	$self->{plotType} = [$self->{plotType}]
		unless ref($self->{plotType}) eq 'ARRAY';
	
	# Bless the array ref into the class and call the class's initialization
	# function:
	bless ($self, $class);
	$self->initialize($array_ref);
	
	return $self;
}

=head2 initialize

Perform any dataset-specific initialization. For piddle-based datasets, this
simply ensures that the data stored in the object are piddles. In other words,
if you pass in a scalar value, it will be converted to a piddle internally, to
simplify the internal routines.

=cut

sub initialize {
	# Not much to do here. This just pulls the x- and y- data out of the passed
	# array reference, packages them in piddles if they are scalars, and stores
	# them in $self:
	my ($dataset, $array_ref) = @_;
	my $xs = $array_ref->[0];
	my $ys = $array_ref->[1];
	eval {
		$xs = pdl($xs) unless ref($xs) eq 'PDL';
		$ys = pdl($ys) unless ref($ys) eq 'PDL';
		$dataset->{xs} = $xs;
		$dataset->{ys} = $ys;
	};
	return unless $@;
	
	croak('For standard datasets, the arguments must be piddles, code references, '
		. 'or scalars that pdl() knows how to process');
}

=head2 draw

Calls all of the drawing functions for each plotType of the dataset. This also
applies all the global drawing options (like C<color>, for example) that were
supplied to the dataset.

=cut

# Calls all the drawing functions for the plotTypes for this dataset:
sub draw {
	my ($dataset, $widget) = @_;
	
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
		$plotType->draw($dataset, $widget);
	}
}

# Returns the data values from the dataset. The widget is required for the
# function-based datasets:
sub get_xs { $_[0]->{xs} }
sub get_ys { $_[0]->{ys} }
sub get_data {
	my ($dataset, $widget) = @_;
	return ($dataset->get_xs($widget), $dataset->get_ys($widget));
}
sub get_data_as_pixels {
	my ($dataset, $widget) = @_;
	
	my ($xs, $ys) = $dataset->get_data($widget);
	return ($widget->x->reals_to_pixels($xs), $widget->y->reals_to_pixels($ys));
}

=head2 extremum

Computes the requested extremum. The syntax looks like this:

 my ($xmin, $padding) = $dataset->extremum('xmin', $graph_widget);

Warning: the following comments almost certainly lead to premature optimizations.

Internally, this uses the comperator for the comparisons. If you write a
function that also uses the comperator, you can save yourself a couple of
compute cycles by passing in the value. That would be the last argument, and it
would be -1 for minima and +1 for maxima. Thus, since I know that I will be
computing a minimum in the previous example, I could rewrite it as

 my ($xmin, $padding) = $dataset->extremum('xmin', $graph_widget, -1);

=cut

sub extremum {
	# This is the object method. It should only be called on objects blessed as
	# Prima::Ex::Graph::Dataset. We need to call the given extremum functions
	# for all of the plotTypes:
	my ($self, $func_name, $widget, $comperator) = @_;
	
	# Determine the comperator if it was not supplied:
	if (not defined $comperator) {
		$comperator = -1;
		$comperator = 1 if $func_name =~ /max$/;
	}
	
	# Different plot types can involve different extrema, or different padding.
	# I will track the most extreme value and largest padding seperately.
	my ($most_extreme, $biggest_padding) = (undef, 0);
	foreach my $plotType (@{$self->{plotType}}) {
		my ($extremum, $padding) = $plotType->$func_name($self, $widget);
		# Undef means that this type cannot determine its extremum. In that
		# case, move to the next data type to see if they have anything to say:
		next unless defined $extremum;
		# Make sure the padding is set to a defined value:
		$padding ||= 0;
		# Keep track of the largest padding:
		$biggest_padding = $padding if $biggest_padding < $padding;
		# Only save the value if it is the most extreme:
		$most_extreme = $extremum
			if not defined $most_extreme
				or ($extremum <=> $most_extreme) == $comperator;
	}
	return ($most_extreme, $biggest_padding);
}

package Prima::Ex::Graph::DataSet::Func;
our @ISA = qw(Prima::Ex::Graph::DataSet);

use Carp 'croak';

# Even less to do for this than for a normal dataset. Just store the function in
# $self and ensure we have a sensible value for N_points:
sub initialize {
	my ($dataset, $array_ref) = @_;
	$dataset->{func} = $array_ref->[0];
	
	# Set the default number of data points (for evaluated data) to 200:
	$dataset->{N_points} ||= 200;
	croak("N_points must be a positive number")
		unless $dataset->{N_points} =~ /^\d+$/ and $dataset->{N_points} > 0
}

sub get_xs {
	my ($self, $widget) = @_;
	
	# working here - implement caching as an option in $self, like this:
	#if ($self->{cacheData}) ...
	return $widget->x->{scaling}->sample_evenly($widget->x->minmax, $self->{N_points});
}
sub get_ys {
	my ($self, $widget) = @_;
	my $xs = $self->get_xs($widget);
	return $self->{func}->($xs);
}

sub get_data {
	my ($dataset, $widget) = @_;
	
	my $xs = $dataset->get_xs($widget);
	return ($xs, $dataset->{func}->($xs));
}

sub extremum {
	my ($self, $func_name, $comperator, $widget) = @_;
	
	# Function-based datasets cannot compute their x extrema, so return undefs
	# for those:
	return (undef, 0) if $func_name =~ /^x/;
	
	# For y extrema, just use the parent class's implementation:
	return $self->SUPER::extremum($func_name, $comperator, $widget);
}

1;
