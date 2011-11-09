package PDL::Graphics::Prima::Simple;
use strict;
use warnings;
use Carp 'croak';

# Import the symbols like pt::Lines and lm::Auto into the global symbol
# table so the user can use them
use PDL::Graphics::Prima::Palette;
use PDL::Graphics::Prima::Limits;
use PDL::Graphics::Prima::Scaling;
use PDL::Graphics::Prima::PlotType;

=head1 NAME

PDL::Graphics::Prima::Simple - a very simple plotting interface

=head1 SYNOPSIS

 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 
 # --( Super simple line and symbol plots )--
 
 # Generate some data - a sine curve
 my $x = sequence(100) / 20;
 my $y = sin($x);
 
 # Draw a line connecting each x/y pair:
 line_plot($x, $y);
 
 # Draw a symbol at each x/y pair:
 circle_plot($x, $y);
 triangle_plot($x, $y);
 square_plot($x, $y);
 diamond_plot($x, $y);
 X_plot($x, $y);
 cross_plot($x, $y);
 asterisk_plot($x, $y);
 
 # Sketch a function:
 func_plot(0, 10, \&PDL::sin);
 
 
 # --( Super simple histogram )--
 
 # PDL hist method returns x/y data
 hist_plot($y->hist);
 my ($bin_centers, $heights) = $y->hist;
 hist_plot($bin_centers, $heights);
 
 
 # --( Super simple matrix plots )--
 
 # Generate some data - a wavy pattern
 my $image = sin(sequence(100)/10)
             + sin(sequence(100)/20)->transpose;
 
 # Generate a greyscale image:
 matrix_plot($image);
 
 # Set the    left, right,  bottom, top
 matrix_plot([0,    1],    [0,      2],  $image);
 
 
 # --( More complex plots )--
 
 # Use the more general 'plot' function for
 # multiple datasets and more plotting features:
 my $colors = pal::Rainbow()->apply($x);
 plot(
     -lines         => [$x, $y],
     -color_squares => [$x, $y + 1, colors => $colors,
                      plotType => pt::Squares(filled => 1)],
     x => { label   => 'Time' },
     y => { label   => 'Sine' },
 );

=head1 DESCRIPTION

C<PDL::Graphics::Prima::Simple> provides the simplest possible interface for
plotting using the L<PDL::Graphics::Prima> graphing widget at the cost of
some of the more powerful aspects of the library. This document not only
explains how to use C<PDL::Graphics::Prima::Simple>, but also provides a
tutorial for understanding the plotting capabilities of L<PDL::Graphics::Prima>.
If you are new to the libarary, this is where you should start.

When you use this library, you get a handful of functions for quick data
plotting, including

=over

=item line_plot ($x, $y)

takes two piddles, one for x and one for y, and makes a line plot with them

=item <symbol>_plot ($x, $y)

takes two piddles (one for x and one for y) and plots the symbol at the
given points

=item func_plot ($x_min, $x_max, $func_ref, [$N_points])

takes an initial plot min/max and a function reference and makes a line plot of
the function; it can optionally take the number of points to plot as well

=item hist_plot ($x, $y)

takes two piddles, one for the bin centers and one for the heights, and plots
a histogram

=item matrix_plot ($xbounds, $ybounds, $matrix)

takes three arguments, the first two of which specify the x-bounds and the
y-bounds, the third of which is the matrix to plot

=item plot (...)

a much more powerful routine that takes the same arguments you would pass to
the constructor of a L<PDL::Graphics::Prima> widget, but which lacks the
flexibility you get with the full GUI toolkit

=back

Calling these functions will create a stand-alone window with the plot that
you want to view. Depending on your operating system and the parameters you
use to load the module, you can either have each window block execution of your
script until you close the window, or you can have them fork a new process,
create a new window, and continue executing the remainder of your script.
Either way, you can create interactive plots in a procedural mindset instead of
a callback/GUI mindset.

The main drawback of using the Simple interface instead of the full-blown
widget interface is that it makes writing custom code to interact with the
user a difficult experience. It also makes animations quite difficult. These
can be done, but if you need any substantial amount of user interaction or
real-time behavior, I suggest you work with the full Prima toolkit.

Although you can plot multiple datasets in the same plot window, a
limitation of this Simple interface is that you cannot create multiple
independent plots in the same window. This is achieved using the full GUI
toolkit by creating two plot widgets, as discussed (somewhere, hopefully)
in L<PDL::Graphics::Prima>.

=head1 INTERACTIVE PLOTTING

Before we get to the plotting commands themselves, I want to highlight that
the L<PDL::Graphics::Prima> library is highly interactive. Whether you use the
widget interface or this Simple interface, your plot responds to the following
user interactions:

=over

=item right-click zooming

Clicking and dragging your right mouse button will zoom into a specific region.
You will see a zoom rectangle on the plot until you release the mouse, at which
point the plot will be zoomed-in to the region that you selected.

=item scroll-wheel zooming

You can zoom-in and zoom-out using your scroll wheel.* The zooming is designed to
keep the data under the mouse at the same location as you zoom in and out.
Unfortunately, some recent changes have made this operation less than perfect.

* This is not working on Windows; I haven't figured out why, but I suspect
it's a Prima thing.

=item dragging/panning

Once you have zoomed into a region, you can examine nearby data by clicking and
dragging with your left mouse button, much like an interactive map.

=item context menu

Right-clicking on the plot will bring up a context menu with options including
restoring auto-scaling, copying the current plot image* (to be pasted directly
into, say, Microsoft's PowerPoint or LibreOffice's Impress), and saving the
current plot image to a file. The supported output file formats depend on the
codecs that L<Prima> was able to install, so are system- and machine-dependent.

* For reasons not clear to me, copying the plot to the clipboard does not
work on Macs. Again, I believe this is a Prima thing and I hope to resolve
it soon becuase I work regularly on a Mac.

=back

=head1 SIMPLEST FUNCTIONS

These functions are bare-bones means for visualizing your data
that are no more than simple wrappers around the more powerful
L<plot|/"PLOT FUNCTION"> function. If you just want to have a quick look at your data, you
should probably start with these. See L<plot|/"PLOT FUNCTION"> if you need more control
over your plot, such as plotting multiple data sets, using variable or
advanced symbols, using multiple plot types, controlling the axis scaling,
using colors, or setting the title or axis labels.

In all of these plots, bad values in x and y are simply omitted.

=over

=item line_plot ($x, $y)

The C<line_plot> function takes two arguments---a piddle with your x data and
a piddle with your y data---and plots them by drawing black lines on a white
background from one point to the next. Usually C<$x> and C<$y> will have the
same dimensions, but you can use any data that are PDL-thread compatible. For
example, here's a way to compare three sets of data that have the exact same
x-values:

 my $x = sequence(100)/10;
 my $y = sequence(3)->transpose + sin($x);
 
 # Add mild linear trends to the first and second:
 use PDL::NiceSlice;
 $y(:, 0) += $x/5;
 $y(:, 1) -= $x/6;
 
 line_plot($x, $y);

The x-values do not need to be sorted. For example, this plots a sine wave sine
wave oscillating horizontally:

 my $y = sequence(100)/10;
 my $x = sin($y);
 line_plot($x, $y);

Bad values in your data, if they exist, will simply be skipped, inserting a
gap into the line.

To generate the same plot using the L<plot|/"PLOT FUNCTION"> command, you would type this:

 plot(-data => [$x, $y]);

=cut

sub line_plot {
	croak("line_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => \@_);
}

=item circle_plot ($x, $y)

Plots filled circles at (x, y). Equivalent L<plot|/"PLOT FUNCTION"> commands
include:

 plot(-data => [$x, $y, plotType => pt::Blobs]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         filled => 'yes',
         N_pionts => 0,
     ),
 ]);

=cut

sub circle_plot {
	croak("circle_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Blobs]);
}

=item triangle_plot ($x, $y)

Plots filled upright triangles at (x, y). Equivalent L<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Triangles(filled => 1)]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         filled => 'yes',
         N_pionts => 3,
         orientation => 'up',
     ),
 ]);

=cut

sub triangle_plot {
	croak("triangle_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Triangles(filled => 1)]);
}

=item square_plot ($x, $y)

Plots filled squares at (x, y). Equivalent L<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Squares(filled => 1)]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         filled => 'yes',
         N_pionts => 4,
         orientation => 45,
     ),
 ]);

=cut

sub square_plot {
	croak("square_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Squares(filled => 1)]);
}

=item diamond_plot ($x, $y)

Plots filled diamonds at (x, y). Equivalent L<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Diamonds(filled => 1)]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         filled => 'yes',
         N_pionts => 4,
     ),
 ]);

=cut

sub diamond_plot {
	croak("diamond_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Diamonds(filled => 1)]);
}

=item cross_plot ($x, $y)

Plots crosses (i.e. plus symbols) at (x, y). EquivalentL<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Crosses]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         N_pionts => 4,
         skip => 0,
     ),
 ]);

=cut

sub cross_plot {
	croak("cross_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Crosses]);
}

=item X_plot ($x, $y)

Plots X symbols at (x, y). EquivalentL<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Xs]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         N_pionts => 4,
         skip => 0,
         orientation => 45,
     ),
 ]);

=cut

sub X_plot {
	croak("X_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Xs]);
}

=item asterisk_plot ($x, $y)

Plots five-pointed asterisks at (x, y). EquivalentL<plot|/"PLOT FUNCTION">
commands include:

 plot(-data => [$x, $y, plotType => pt::Asterisks(N_points => 5)]);
 plot(-data => [
     $x,
     $y,
     plotType => pt::Symbol(
         N_pionts => 5,
         skip => 0,
         orientation => 'up',
     ),
 ]);

=cut

sub asterisk_plot {
	croak("asterisk_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Asterisks(N_pionts => 5)]);
}


=item func_plot ($x_min, $x_max, $func_ref, [$N_points])

The C<func_plot> function takes three or four arguments and plots a function.
The first two arguments are the initial x-bounds (min and max); the third
argument is the function that you want to plot; the optional fourth argument is
the number of points that you want to use in generating the plot. The resulting
figure will have a black line drawn against a white background.

The function itself will be called whenever the plot needs to be redrawn. It
will be passed a single argument: a piddle with sequential x-points at which
the function should be evaluated. The function should return a piddle of
y-points to be plotted. Here are some examples:

 # Plot PDL's exponential function:
 func_plot (1, 5, \&PDL::exp);
 
 # Plot a rescaled tangent function:
 func_plot (1, 5, sub {
     my $xs = shift;
     return 5 * ($xs / 4)->tan
 });
 # or equivalently, if you "use PDL":
 func_plot (1, 5, sub {
     my $xs = shift;
     return 5 * tan($xs / 4);
 });

Your function can return bad values, in which case they will not be drawn. For
example, here is a function that plots a decaying exponential, but only for
values of x greater than or equal to zero. It starts with an initial view of
x running from 0 to 4:

 func_plot (0, 4, sub {
     my $xs = shift;
     my $ys = exp(-$xs);
     return $ys->setbadif($xs < 0);
 });

The return value must have thread-compatible dimensions, but that means that they
do not need to be identical to the input x. For example, a scalar is
thread-compatible with a vector, so the following will create a line at a
y-value of 1:

 func_plot (0, 4, sub { 1 });

Or, you can return a piddle with more dimensions than the input:

 func_plot (0, 4, sub {
     my $xs = shift;
     my $first_ys = sin($xs);
     my $second_ys = cos($xs) + 3;
     return $first_ys->cat($second_ys);
});


If you do not specify the number of points to draw, the equivalent L<plot|/"PLOT FUNCTION">
command is this:

 plot(
     -data => [$func_ref],
     x => { min => $xmin, max => $xmax },
 );

If you do specify the number of points to draw, the equivalent L<plot|/"PLOT FUNCTION"> command
is this:

 plot(
     -data => [$func_ref, N_points => $N_points],
     x => { min => $xmin, max => $xmax },
 );

=cut

sub func_plot {
	croak("func_plot expects three or four arguments: the x min and max, the function reference,\n"
		. "   and, optionally, the number of points to plot")
		unless @_ == 3 or @_ == 4;
	my ($xmin, $xmax, $func_ref, $N_points) = @_;
	plot(
		-data => [$func_ref, ($N_points ? (N_points => $N_points) : ())],
		x => {
			min => $xmin,
			max => $xmax,
		},
	);
}

=item hist_plot ($x, $y)

The C<hist_plot> function takes two arguments, the centers of the histogram
bins and their heights. It plots the histogram as black-outlined rectangles
against a white background. As with the other functions, C<$x> and C<$y>
must be thread-compatible. C<hist_plot> is designed to play very nicely with
PDL's L<hist|PDL::Basic/"hist"> function, so the following idiom works:

 my $data = grandom(400);
 hist_plot($data->hist);

PDL's L<hist|PDL::Basic/"hist"> function does not (to my knowledge) generate bad
values, but if you generate your values by some other means and any heights or
positions are bad the will be skipped, leaving a gap in the histogram.

The equivalent L<plot|/"PLOT FUNCTION"> command is:

 plot(-data => [$x, $y, plotType => pt::Histogram]);
 

=cut

sub hist_plot {
	croak("hist_plot expects two piddles, the bin-centers and the bin heights")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Histogram]);
}

=item matrix_plot ([$xbounds, $ybounds,] $matrix)

The C<matrix_plot> function takes either one or three arguments. The first designates the
x-bounds of the plot; the second designates the y-bounds of the plot, and
the third specifies a matrix that you want to have plotted in grey-scale.
The x-bounds and y-bounds arguments should either be two-element arrays or
two-element piddles designating the min and the max. (You can also pass
arrays or piddles with more elements, but I will defer discussing that until
later.)

Bad values, if your data has any, are skipped. This means that you will have
a white spot in its place (since the background is white), which is not
great. Future versions may use a different color to specify bad values.

If you do not specify the x- and y-bounds, the equivalent L<plot|/"PLOT FUNCTION"> command is:

 plot(-image => [plotType => pt::ColorGrid(colors => $matrix)]);

If you do specify the x- and y-bounds, the quivalent is:

 plot(-image => [$x_bounts, $y_bounts
               , plotType => pt::ColorGrid(colors => $matrix)]
 );

=cut

sub matrix_plot {
	my ($x, $y, $matrix);
	if (@_ == 1) {
		$x = [0, 1];
		$y = [0, 1];
		($matrix) = @_;
	}
	elsif (@_ == 3) {
		($x, $y, $matrix) = @_;
	}
	else {
		croak("matrix_plot expects an image, optionally preceeded by its x- and y-limits:\n"
			. "matrix_plot ([x0, xf], [y0, yf], \$image)")
	}
	
	plot(-image => [$x, $y, plotType => pt::ColorGrid(colors => $matrix)]);
}

=back

=head1 PLOT FUNCTION

The C<plot> function is the real workhorse of this module. Not only does it
provide the functionality behind all of the above simple functions, but it
also lets you plot multiple datasets, specify axis labels and a plot title,
direct the axis scaling (linear or logarithmic), and set many other properties
of the plot.

Arguments that you pass to this function are almost identical to the arguments
that you would use to create a Plot widget, so it serves as an excellent sandbox
for playing with the widget's constructor. Also, once you understand how to use
this function, using the actual widget in an interactive GUI script is simply a
matter of understanding how to structure a GUI program.

The C<plot> function takes options and dataset specifications as key/value
pairs. The basic usage of plot looks like this:

 plot(
     -dataset1 => [$x1, $y1, ...options...],
     x => {
     	other => options,
     },
     -dataset2 => [$x2, $y2, ...options...],
     y => {
     	axis => options,
     },
     -mydata => [$data_a, $data_b, ...options...],
     title => 'Title!',
     titleSpace => 60,
     -more_data => [$x_a, $y_a],
     ... Prima Drawable options ...
 );

Notice that some of the keys begin with a dash while others do not. Any key
that begins with a dash is a dataset. You can use any name that you wish for
your datasets, the only requirement is that the name begins with a dash. The
keys that do not begin with a dash are Plot options. The Plot widget has a
handful of Plot-specific properties, but you can also specify any property of a
L<Prima::Widget> object.

In the world of C<PDL::Graphics::Prima>, the fundamental object is the Plot.
Each plot can hold one or more L<PDL::Graphics::Prima::DataSet>s, and each
DataSet is visualized using one or more L<PDL::Graphics::Prima::PlotType>s.
This makes the plotType the simplest element to discuss, so I'll start there.

=head2 Plot Types

Each dataSet can have one or more plotTypes. If you only want to specify a
single plotType, you can do so by specifying it after the plotType key for your
dataset:

 -data => [
     ...
     plotType => pt::Squares,
     ...
 ]

You can specify multiple plotTypes by passing them in an anonymous array:

 -data => [
     ...
     plotType => [pt::Triangles, pt::Lines],
     ...
 ]

All the plotTypes take key/value paired arguments. You can specify various
L<Prima::Drawable> properties like line-width using the C<lineWidths> key
or color using the C<colors> key; you can pass plotType-specific options
like symbol size (for C<pt::Symbol> and itsd derivatives) using the C<size> key
or the baseline height for C<pt::Histogram> using the C<baseline> key; and
some of the plotTypes have required arguments, such as at least one error bar
specification with C<pt::ErrorBars>. To create red blobs, you would use
something like this:

 pt::Blobs(colors => cl::LightRed)

To specify a 5-pixel line width for a Lines plotType, you would say

 pt::Lines(lineWidths => 5)

(Notice that these keys are identical to the properties listed in L<Prima::Drawable>,
except that they are plural. Plural keys means you can specify a piddle for the
values and it will thread over the piddle while it threads over the drawing.
At the moment, singular keys do not work with plotTypes.)

When a dataset gets drawn, it draws the different plotTypes in the order
specified. For example, suppose you specify C<cl::Black> filled triangles and
C<cl::LightRed> lines. If the triangles are specified first, they will have red
lines drawn through them, and if the triangles are second, the triangles will
be drawn over the red lines.

The default plot type is C<pt::Lines> so you do not need to specify that if you
simply want lines drawn from one point to the next. The plotTypes are discussed
thoroughly in L<PDL::Graphics::Prima::PlotType>, and are summarized below:

 pt::Lines     - lines from point to point
 pt::Blobs     - blobs (filled ellipses) with specifiable x- and y- radii
 pt::Symbols   - open or filled regular geometric shapes with many options:
                 size, orientation, number of points, skip pattern, and fill
 pt::Triangles - open or filled triangles with  specifiable
               - orientations and sizes
 pt::Squares   - open or filled squares with specifiable sizes
 pt::Diamonds  - open or filled diamonds with specifiable sizes
 pt::Stars     - open or filled star shapes with specifiable sizes,
                 orientations, and number of points
 pt::Asterisks - asterisk shapes with specifiable size, orientation, and
                 number of points
 pt::Xs        - four-point asterisks that look like xs, with specifiable
                 sizes
 pt::Crosses   - four-pint asterisks that look like + signs, with
                 specifiable sizes
 pt::Spikes    - spikes to (x,y) from a specified vertical or horizontal
                 baseline
 pt::Histogram - histograms with specifiable baseline and top padding
 pt::ErrorBars - error bars with specified x/y errors and cap sizes
 pt::ColorGrid - colored rectangles, i.e. images

=head2 Data Sets

You can plot one or more sets of data on a given Plot. You do this by specifying
the dataset's name with a dash, followed by an anonymous array with the
properties of your dataset (including the plotType, as already discussed).
There are two kinds of datasets that you can specify: data-based and
function-based. Data-based datasets are what you would use when you have
explicit x/y data that needs to be visualized. Function-based datasets are what
you would use when you want to visualize functions, such as fit functions:

 -data => [ $x, $y, ... ],
 -func => [ $func_ref, ... ],

The difference between the two is that function-based datasets have a CODE
reference as its first argument, while data-based datasets do not. In fact,
data-based datasets do not need to have piddles for their first two arguments.
Anything that can be converted to a piddle, including scalar numbers and
anonymous arrays of values, can be specified as the first two arguments for
data-based datasets. That means that the following are valid dataset
specifications:

 # Data based:
 -data => [ sequence(10), sequence(10)->sin ]
 -data => [ [1, 2, 3], [1, 4, 9] ]
 -data => [ sequence(100), 5 ]
 
 # Function based:
 -data => [ sub {return 5} ]
 -data => [ sub {return $_[0]**2} ]
 -data => [ \&PDL::sin ]

You don't have to sort the data that you use, but Lines, Histograms, and
ColorGrids might look very strange if you don't. Then again, they may look
exactly how you want them to look. It depends what you're trying to accomplish.

Once you have specified the data or function that you want to plot, you can
specify other options with key/value pairs. I discussed the
plotType key already, but you can also specify any property in L<Prima::Drawable>.
When you specify properties from L<Prima::Drawable>, these become the default
parameters for all the plotTypes that belong to this dataset. For example, you
can specify a default color as C<cl::LightRed>, and then the lines, blobs, and
error bars will be drawn in red unless they override the colors themselves.
Function-based datasets also recognize the C<N_points> key, which indicates the
number of points to use in evaluating the function.

To get an idea of how this works, suppose I have some data that I want to
compare with a fit. In this case, I would have two datasets, the data (plotted
using error bars) and the fit (plotted using a line). I would plot this data
with code like so:

 plot(
     # The experimental data
     -data => [
         $x,
         $y,
         # I want error bars along with squares:
         plotType => [
             pt::ErrorBars(y_err => $y_errors),
             pt::Squares(filled => 1),
         ],
     ],
     
     # The linear fit:
     -fit => [
         sub {
             return $y_intercept + $slope * $_[0];
         },
         # Default plotType is lines, but I'll be explicit:
         plotType => pt::Lines,
     ],
 );

The part C<< -data => [...] >> specifies the details for how you want to plot
the experimental data and the part C<< -fit >> specifies the details for how you
want to plot the fit. 

The datasets are plotted in ASCIIbetical order, which means that in the example
above, the fit will be drawn over the error bars and squares. If you want the data
plotted over the fit curve, you should choose different names so that they sort
the way you want. For example, using C<-curve> instead of C<-fit> might work.
So would changing the names from C<-data> and C<-fit> to C<-b_data> and
C<-a_fit>, respectively.

=head2 Plot Options

Finally we come to setting plot-wide properties. As already discussed, you can
disperse datasets among your other Plot properties. Plot-wide properties include
the title and the amount of room you want for the title (called titleSpace),
the axis specifications, and any default L<Prima::Drawable> properties that you
want applied to your plot.

The text for your plot's title should be a simple Perl string. UTF8 characters
are allowed, which means you can insert Greek or other symbols as you need them.
However, L<Prima>, and therefore L<PDL::Graphics::Prima>, does not support fancy
typesetting like subscripts or superscripts. (If you want that in the Plot
library, you should probably consider petitioning for and helping add that
functionality to Prima. Open-source is great like that!) The amount of space
allocated for the title is currently set at 80 pixels. You can specify a
different size if you prefer. (It should probably be calculated based on the
current font-size---Prima makes it relatively easy to do that---but that's not 
yet implemented.) Space is allocated for the title only when you specify one;
if none is specified, you have more room for your plot.

Axis labels have similar restrictions and capabilities as the title string, but
are properties of the axes themselves, which additionally have specifiable
bounds (min and max) and scaling type. At the moment, the only two scaling types
are C<sc::Linear> and C<sc::Log>. The bounds can be set to a specific numeric
value, or to C<lm::Auto> if you want the bounds automatically computed based on
your data and plotTypes.

Finally, this is essentially a widget constructor, and as such you can specify
any L<Prima::Widget> properties that you like. These include all the properties in
L<Prima::Drawable>. For example, the default background color is white, because
I like a white background on my plots. If you disagree, you can change the widget's
background and foreground colors by specifying them. The dataSets and their
plotTypes will inherit these properties (most importantly, the foreground color)
and use them unless you override those properties seperately.

=head2 Examples

This first example is a simple line plot with triangles at each point. There's only
one dataset, and it has only two plotTypes:

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = sin($x);
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Triangles,
             pt::Lines,
         ],
     ],
 );

Now for something more fun. This figure uses bright colors and random circle radii.
Notice that the lineWidth of 3 obscures many of the circles since their radii are
between 1 and 5. This has only one dataset and two plotTypes like the
previous example. In contrast to the previous example, it specifies a number of
properties for the plotTypes:

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = sin($x);
 my $colors = pal::Rainbow->apply($y);
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Blobs (
                 radius => 1 + $x->random*4,
                 colors => $colors,
             ),
             pt::Lines (
                 lineWidths => 3,
             ),
         ],
     ],
 );

Here I use a black background and white foreground, and plot the circles B<over>
the line instead of under it. I achieve this by changing the order of the
plotTypes---Lines then Blobs. Also notice that I use the C<-sequential> flag,
which I have to do in order to guarantee having Prima's color shortcuts.

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple -sequential;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = sin($x);
 my $colors = pal::Rainbow->apply($y);
 my $radius = 1 + $x->random*4;
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Lines (
                 lineWidths => 3,
             ),
             pt::Blobs (
                 radius => $radius,
                 colors => $colors,
             ),
         ],
     ],
     backColor => cl::Black,
     color => cl::White,
 );

I find the smaller points very difficult to see, so here's a version in which I
'wrap' the points with a white radius. I achieve that by drawing two consecutive
blobs, the first of which is the default color and which has a radius one larger
than the colored radius. Of course, it might be better to use a plotType that
makes open circles, but that's not (yet?) available.

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple -sequential;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = sin($x);
 my $colors = pal::Rainbow->apply($y);
 my $radius = 1 + $x->random*4;
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Lines (
                 lineWidths => 3,
             ),
             pt::Blobs(
                 radius => 1 + $radius,
             ),
             pt::Blobs (
                 radius => $radius,
                 colors => $colors,
             ),
         ],
     ],
     backColor => cl::Black,
     color => cl::White,
 );

Here I use PDL threading to achieve the same ends as the previous example, but
only using one Blobs plotType instead of two.

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple -sequential;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = sin($x);
 my $rainbow_colors = pal::Rainbow->apply($y);
 my $whites = $y->ones * cl::White;
 my $colors = cat($whites, $rainbow_colors);
 my $inner_radius = 1 + $x->random*4;
 my $radius = cat($inner_radius + 1, $inner_radius);
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Lines (
                 lineWidths => 3,
             ),
             pt::Blobs(
                 radius => $radius,
                 colors => $colors,
             ),
         ],
     ],
     backColor => cl::Black,
     color => cl::White,
 );

Here I generate some linear data with noise and perform a least-squares fit to
it. In this case I perform the least-squares fit by hand, since not everybody
will have Slatec installed. The important part is the C<use PDL::Graphics::Prima::Simple>
line and beyond. Also notice that I construct the function color based on
L<PDL::Drawing::Prima>'s C<rgb_to_colors> function so that I can run this code
without needing to use Prima's constant C<cl::LightRed>.

This example demonstrates the use of multiple data sets, the use of the
ErrorBars plotType and the use of function-based data sets.

 use strict;
 use warnings;
 use PDL;
 
 my $x = sequence(100)/10;
 my $y = $x/2 - 3 + $x->grandom*3;
 my $y_err = 2*$x->grandom->abs + 1;
 
 # Calculate the slope and intercept:
 my $S = sum(1/$y_err);
 my $S_x = sum($x/$y_err);
 my $S_y = sum($y/$y_err);
 my $S_xx = sum($x*$x/$y_err);
 my $S_xy = sum($x*$y/$y_err);
 my $slope = ($S_xy * $S - $S_x * $S_y) / ($S_xx * $S - $S_x * $S_x);
 my $y0 = ($S_xy - $slope * $S_xx) / $S_x;
 
 
 use PDL::Graphics::Prima::Simple;
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Diamonds(filled => 'yes'),
             pt::ErrorBars(y_err => $y_err),
         ],
     ],
     -func => [
         sub { $y0 + $slope * $_[0] },
         lineWidth => 2,
         color => pdl(255, 0, 0)->rgb_to_color,
     ],
 );

Trying to extend the Simple interface with GUI methods is limited but can be
very useful. The next example gives you a first glimps into GUI-flavored
programming by overriding the onMouseMove method for this Prima widget. There
are many ways of setting, overriding, or adding callbacks in relation to all
sorts of GUI events, but when using the C<plot> command, your only option is to
specify it as OnEventName.

In this example, I add (that's B<add>, not override, becaus Prima rocks) a
method that prints out the mouse position to the console. I also use logarithmic
scaling in the x direction (and specify x- and y-axes labels) to make things
interesting:

 use strict;
 use warnings;
 use PDL::Graphics::Prima::Simple;
 use PDL;
 
 # Turn on autoflush:
 $|++;
 
 my $x = sequence(100)/10 + 1;
 my $y = sin($x);
 
 plot(
     -data => [
         $x,
         $y,
         plotType => [
             pt::Xs,
             pt::Lines,
         ],
     ],
     onMouseMove => sub {
         # Get the widget and the coordinates:
         my ($self, $button_and_modifier, $x_pixel, $y_pixel) = @_;
         # Convert the pixel coordinates to real values:
         my $x = $self->x->pixels_to_reals($x_pixel);
         my $y = $self->y->pixels_to_reals($y_pixel);
         # Print the results:
         print "\r($x, $y)           ";
     },
     x => {
         label => 'time (s)',
         scaling => sc::Log,
     },
     y => {
         label => 'displacement (m)',
     }
 );

This kind of immediate feedback can be very useful. It is even possible to
capture keyboard events and respond to user interaction this way. But B<please>,
don't do that. If you need any substantial amount of user interaction, you
would do much better to learn to create a Prima application with buttons, lists,
and input lines, along with the Plot widget. For that, see L<PDL::Graphics::Prima>.

=cut

# A function that allows for quick one-off plots:
sub plot {
	# Make sure they sent key/value pairs:
	croak("Arguments to plot must be in key => value pairs")
		unless @_ % 2 == 0;
	my %args = @_;

	if (defined $::application) {
		# The Prima application is already running, which means we're going to
		# do blocking behavior. For a myriad of reasons, PDL::Graphics::Prima
		# may not have been loaded yet, so be sure to load it:
		if (not defined $PDL::Graphics::Prima::VERSION) {
			require PDL::Graphics::Prima;
			PDL::Graphics::Prima->import();
		}
#		if (not defined $Prima::Application::uses) {
#			# Make sure the have the Application stuff loaded:
#			Prima->import( qw(Application));
#		}
		
		# Since the application is already running, then simply create a new
		# window:
		my $window = Prima::Window->create(
			text  => $args{title} || 'PDL::Graphics::Prima',
			size  => $args{size} || [our @default_sizes],
		);

		$window->insert('Plot',
			pack => { fill => 'both', expand => 1},
			%args
		);
		
		# Create a plot window and block until the user closes it
		$window->execute;
		$window->destroy;
	}
	else {

		my $pid = fork();
		die "cannot fork" unless defined $pid;
		
		if ($pid == 0) {
			$SIG{TERM} = sub {
				exit;
			};
			
			# child process, create the plot
			require 'Prima.pm';
			Prima->import('Application');
			require 'PDL/Graphics/Prima.pm';
			PDL::Graphics::Prima->import();
			
			my $wDisplay = Prima::MainWindow->create(
				text  => $args{title} || 'PDL::Graphics::Prima',
				size  => $args{size} || [our @default_sizes],
			);
			
			$wDisplay->insert('Plot',
				pack => { fill => 'both', expand => 1},
				%args
			);

			# Display the plot and exit when done:
			run Prima;
			
			exit;
		}
	}
}

=head1 IMPORTED METHODS

First, don't C<use Prima> in scripts that use C<PDL::Graphics::Prima::Simple>.
That might make things blow up. I wish I knew what goes wrong so I could guard
against it, but I haven't figured it out yet.

There are a couple of ways to call this module. The first is just a simple
use statement:

 use PDL::Graphics::Prima::Simple;

This will import the above mentioned functions into your local package's
symbol table, which is generally what you want. If you do not want any
functions imported, you can call it with an empty string:

 use PDL::Graphics::Prima::Simple '';

or you can name the functions that you want imported:

 use PDL::Graphics::Prima::Simple qw(plot);

In addition, you can specify the default window plot size by passing a two
element anonymous array:

 # default to 800 x 800 window:
 use PDL::Graphics::Prima::Simple [800, 800];
 
 # default to 800 wide by 600 tall, import nothing:
 use PDL::Graphics::Prima::Simple [800, 600], '';
 
 # default to 300 wide by 450 tall, import 'plot' function:
 use PDL::Graphics::Prima::Simple [300, 450], 'plot';

Finally, the default behavior on a Unix-like system that supports proper
forking (as discussed L<below|/"BLOCKING, NONBLOCKING, AND PRIMA CONSTANTS">)
is to create a stand-alone plot window that you can manipulate while the rest
of your script runs, and which persists even after your script exits.
Unfortunately, Prima doesn't like forking, so although this works well with
pdl shell usage, it comes with the caviate that you do not have access to
any Prima constants, most importantly, the color constants. If you need
access to these constants, or if you
want the script to block after each function call until the plot window closes,
you can provide the C<-sequential> switch:

 # force sequential (i.e. blocking) behavior
 use PDL::Graphics::Prima::Simple -sequential;
 
 # 300 wide by 450 tall, sequential behavior:
 use PDL::Graphics::Prima::Simple [300, 450], -sequential;
 
 # same as above; order doesn't matter:
 use PDL::Graphics::Prima::Simple -sequential, [300, 450];

(If your goal is to create a script that runs B<identically> on all supported
platforms, you should always use this switch, unless I figure out how to
properly fork a seperate process under Windows.)

=cut

# import/export stuff:
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = our @EXPORT = qw(plot line_plot circle_plot triangle_plot
	square_plot diamond_plot cross_plot X_plot asterisk_plot hist_plot matrix_plot func_plot);

our @default_sizes = (400, 400);

# Override the import method to handle a few user-specifiable arguments:
sub import {
	my $package = shift;
	my $sequential;
	my @args;
	# Run through all the arguments and pull out anything special:
	foreach my $arg (@_) {
		# See if they passed in an array reference
		if(ref ($arg) and ref($arg) eq 'ARRAY') {
			# Make sure it is the correct size:
			croak("Array references passed to PDL::Graphics::Prima::Simple indicate the\n"
				. "desired plot window size and must contain two elements")
				unless @$arg == 2;
			
			# Apparently we're good to go so save the sizes:
			@default_sizes = @$arg;
		}
		elsif ($arg eq '-sequential') {
			$sequential++;
		}
		else {
			push @args, $arg;
		}
	}
	
	$package->export_to_level(1, $package, @args);

	# Set up Prima for sequential plotting if that's what they want/get
	if($sequential or defined $::application or $^O =~ /MS/) {
		# If this is windows, we'll use a single application that gets managed
		# by the various plot commands:
		require 'Prima.pm';
		Prima->import(qw(Application));
	}
}

1;

=head1 BLOCKING, NONBLOCKING, AND PRIMA CONSTANTS

Under Unix-like systems that support proper forking, namely Linux and Mac
OSX (and Unix I suspect, though I cannot say for sure), the default behavior is
to fork a process that creates the plot window
and immediately resumes the execution of your code. These windows remain
open even after your script exits, which can be handy in a number of
circumstances. In particular, it is exactly the behavior you want when you use
this module with the perldl shell, in which case making plots with these
commands will give you fully interactive plot windows and a fully working
(i.e. non-blocked) shell.

The drawback of this method is that Prima's constants, like C<cl::Red> for the
red color value or C<lp::ShortDash> for the short-dash line pattern (i.e.
line-style) are not accessible. Although there are ways of generating these
colors and line styles using methods from L<PDL::Drawing::Prima>, the lack of
shorthand notation when using the nonblcoking plotting is a bummer. The
"solution" to this is to use the C<-sequential> argument when you C<use> this
module.

When run under Windows or if you supply the C<-sequential> argument when you
C<use> this module, the plot commands will block until they are closed. This
can be helpful if you want to view a series of plots and you want to enforce
a no-clutter policy, but it does not play so well with the mental model
underlying the perldl shell. Unfortuntely, I know of no way to get
nonblocking behavior on Windows. Patches welcome!

=head1 LIMITATIONS AND BUGS

A major limitation of this module is that you can't C<use Prima> in your
scripts. This should essentially be an implicit C<-sequential>, but I can't find
a way to get it to work correctly. Another limitation is that in nonblocking
mode, you don't have access to the Prima constants. I have not determined an
adequate solution to this rather vexing problem.

I am sure there are bugs in this software. If you find them, you can report them
in one of three ways:

=over

=item PDL Mailing List

You can (and should!) join the PDL mailing list, and you should feel free to
post questions about this or any other PDL module on that list. For details on
how to sign-up, see L<http://pdl.perl.org/?page=mailing-lists>.

=item Github

The best place to report problems that you are sure are problems is at
L<http://github.com/run4flat/PDL-Graphics-Prima/issues>.

=item CPAN RT

As this is Perl, this module will have an RT tracker. The link for this will
only be available after this module has been uploaded to CPAN (as of this writing,
it has not been uploaded). Once that has happened, I'll add a link to it here.

=back

=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 SEE ALSO

For an introduction to L<Prima> see L<Prima::tutorial>.

This is a component of L<PDL::Graphics::Prima>, a library composed of many
modules:

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
