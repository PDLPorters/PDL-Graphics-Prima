use strict;
use warnings;

package Prima::Ex::Graph;
use base 'Prima::Widget';

=head1 NAME

Prima::Ex::Graph - a graphing extension for Prima

=head1 SYNOPSIS

 working here

=head1 STEPS

These are the things I need to do to have a bonafide widget:

=over

=item decide on a set of properties

For now, I'm going to keep everything in one big widget. This means all of the
axis settings, for example, will be properties of this widget. What should those
include?

logarithmic or linear scaling, axis limits, tick mark details, axis label text
and positioning, data.

I have an idea for the data that I think will make it easy. Adding a data set
involves passing an anonymous array with the arguments that you would have
supplied to the polylines routine. The drawing of this data will then simply
involve rescaling the values to the axis limits and calling the polylines
function.

=item write accessors 

=item create a default profile

=item write initialization and destruction code (if there is anything special)

=item set up any notifications

=back

=head1 PROPERTIES

These are the items that you can easily get or set.

=head2 xleftBoundary, xrightBoundary, ybottomBoundary, ytopBoundary,
xboundaries, yboundaries, boundaries

The boundaries of the plotting region. The can be either percentages or pixel
counts. Both must be passed in as strings, since raw numbers refer to coordinate
values for other properties, but coordinate values are not defined for setting
boundaries.

For example:

 $graph->xleftBoundary('10%');
 # the next two are equivalent:
 $graph->yboundaries('10%', '90%');
 $graph->yboundaries('10%');

For setters that set more than one boundary, specifying on a single string
will result in that string's complement being used for the opposit boundary.

This could probably make use of the various geometry options available in Prima
to set flexible minima and maxima, but those will have to wait.

=cut

sub good_boundary {
	my $boundary = shift;
	
	if ($boundary !~ /^\d+(%|p)$/) {
		# bad, set $@
		return 0;
	}
	
	return 1;
}

sub xleftBoundary {
	my ($self, $xlb) = @_;
	return $self->{x_left_boundary} unless defined $xlb;
	if (not good_boundary($xlb)) {
		croak($@);
	}
}

sub viewport {
	my $self = shift;
	return @{$self->{viewport}} unless $#_;
	croak("Viewport must be a four-element array reference")
		unless ref($new_viewport) and ref($new_viewport) eq 'ARRAY'
			and @$new_viewport == 4;
	foreach(@$new_viewport) {
		croak("Viewport values must be between "
	}
	$self->{viewport} = $new_viewport;
}

=head2 TickLength, tickLength, xTickLength, yTickLength

Major and minor tick lengths. These are specified with strings or with anonymous
arrays with pairs of strings. The strings can have one of three formats:

 Length as a percentage of the viewport: "2%"
 Length in pixels: "5p"
 Length in coordinates: 1.2

Lengths can be positive or negative. Positive lengths are drawn within the
viewport whereas negative lengths are drown outside the viewport. If you specify
a string pair using an anonymous array, you indicate where the tick starts and
where the tick stops, so that you can have ticks both below and above the axis,
or floating away from the axis, if you like.

Examples:

 # Sets ticks both inside and outside the viewport
 $graph->TickLength(['-2%', '2%')]);

=head2 xTickStep, yTickStep 

Controls the step size between major tick marks. As with tick length, these can
be specified in one of three ways: a percentage, a number of pixels, or a
coordinate distance. However, these must be strictly positive.

=head2 xtickCount, ytickCount

Sets the number of minor ticks between major ticks.

=head2 xmin, xmax, ymin, ymax, xminmax, yminmax, minima, maxima, limits

These set the bounds of the plot. The can take real values, the string 'fixed',
or strings with a percent at the end. For example, to view all your data,
automatically adjusting even as you added new data, you would use this:

 $graph->limits('0%', '0%', '100%', '100%');

If you like the x-maximum and would like to keep the graph at the current value,
you would use:

 $graph->xmax('fixed');

Adding new data will not change the xmax.

=head2 xlabel, ylabel

Text strings indicating the labels.

=head2 xlabelPosition, ylabelPosition

Positioning settings. I should read more about the text positioning commands
before I push to far along these.

=cut



# for the tick marks, the code will look something like this:
if (ref{$self->{ticks}} eq 'CODE') {
	$self->{ticks}->($canvas, )
}
