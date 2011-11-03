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
 
 
 # --( Super simple line and blob plots )--
 
 # Generate some data - a sine curve
 my $x = sequence(100) / 20;
 my $y = sin($x);
 
 # Draw a point at each x/y pair:
 blob_plot($x, $y);
 
 # Draw a line connecting each x/y pair:
 line_plot($x, $y);
 
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
     -lines       => [$x, $y],
     -color_blobs => [$x, $y + 1, colors => $colors,
                      plotType => pt::Blobs],
     x => { label => 'Time' },
     y => { label => 'Sine' },
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

=item blob_plot ($x, $y)

takes two piddles, one for x and one for y, and makes a blob plot with them

=item line_plot ($x, $y)

takes two piddles, one for x and one for y, and makes a line plot with them

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

Before we get to the plotting command themselves, I wanted to highlight that
the L<PDL::Graphics::Prima> library is highly interactive. Whether you use the
widget interface or this Simple interface, your plot responds to the following
user interactions:

=over

=item right-click zooming

Clicking and dragging your right mouse button will zoom into a specific region.
You will see a zoom rectangle on the plot until you release the moust, at which
point the plot will be zoomed-in to the region that you selected.

=item scroll-wheel zooming

You can zoom-in and zoom-out using your scroll wheel. The zooming is designed to
keep the data under the mouse at the same location as you zoom in and out.
(Unfortunately, some recent changes have made this operation less than perfect.
See the L<"WARTS"> section below. working here - make sure I have the correct
link)

=item dragging/panning

Once you have zoomed into a region, you can examine nearby data by clicking and
dragging with your left mouse button, much like an interactive map.

=item context menu

Right-clicking on the plot will bring up a context menu with options including
restoring auto-scaling, copying the current plot image (to be pasted directly
into, say, Microsoft's PowerPoint or LibreOffice's Impress), and saving the
current plot image to a file. The supported output file formats depend on the
codecs that L<Prima> was able to install, so are system- and machine-dependent.

=back

=head1 SIMPLEST FUNCTIONS

These functions are bare-bones means for visualizing your data
that are no more than simple wrappers around the more powerful
L<plot|/"PLOT FUNCTION"> function. If you just want to have a quick look at your data, you
should probably start with these. See L<plot|/"PLOT FUNCTION"> if you need more control
over your plot, such as plotting multiple data sets, using multiple plot
types, controlling the axis scaling, or setting the title or axis labels.

=over

=item blob_plot ($x, $y)

The C<blob_plot> function
takes two arguments, your x-data and your y-data. It then plots black dots
against a white background. See the Synopsis for a simple example, and see
the L<line_plot> example for how to plot multidimensional data.

Bad values in your data are simply omitted from the plot.

The equivalent L<plot|/"PLOT FUNCTION"> command is:

 plot(-data => [$x, $y, plotType => pt::Blobs]);

=cut

sub blob_plot {
	croak("blob_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Blobs]);
}

=item line_plot ($x, $y)

Like the L<blob_plot> function just discussed, the C<line_plot> function takes
two arguments---a piddle with your x data and
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
pairs. 

datasets
 - plot styles
axis specifications
 label
 scaling type
 bounds
colors
lineWidths
lineStyles

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
against it, but I haven't figure dit out yet.

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
of your script runs, and which persists even after your script exits. If you
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
our @EXPORT_OK = our @EXPORT = qw(plot line_plot hist_plot blob_plot matrix_plot func_plot);

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

The full plot widget, upon which this module is built, is documented under
L<PDL::Graphics::Prima>.

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2011 David Mertens. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
