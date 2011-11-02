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
 
 
 # --( Super simple histogram )--
 
 # PDL hist method returns x/y data
 hist_plot(	$y->hist);
 hist_plot($bin_centers, $heights);
 
 # --( Super simple matrix plots )--
 
 # Generate some data - a wavy pattern
 my $image = sin(sequence(100)/10)
             + sin(sequence(100)/20)->transpose;
 
 # Generate a greyscale image:
 #           x0, xf   y0 yf  image
 matrix_plot([0,  1], [0, 2], $image);
 
 
 # --( More complex plots )--
 
 # Use the more general plot for multiple datasets
 # and more plotting features:
 my $colors = pal::Rainbow()->apply($x);
 plot(
     -lines       => [$x, $y],
     -color_blobs => [$x, $y + 1, colors => $colors,
                      plotType => pt::Blobs],
     xLabel => 'Time',
     yLabel => 'Sine',
 );

=head1 DESCRIPTION

C<PDL::Graphics::Prima::Simple> provides the simplest possible interface for
plotting using the C<PDL::Graphics::Prima> graphing widget at the cost of
some of the more powerful aspects of the library. When you use this library,
you get a handful of functions for quick data plotting, including

=over

=item line_plot ($x, $y)

takes two piddles, one for x and one for y, and makes a line plot with them

=item blob_plot ($x, $y)

takes two piddles, one for x and one for y, and makes a blob plot with them

=item hist_plot ($x, $y)

takes two piddles one for the bin centers and one for the heights, and plots
a histogram

=item matrix_plot ($xbounds, $ybounds, $matrix)

takes three arguments, the first two of which specify the x-bounds and the
y-bounds, the third of which is the matrix to plot

=item plot (...)

a much more powerful routine that takes the same arguments you would pass to
the constructor of a C<PDL::Graphics::Prima> widget, but which lacks the
flexibility you get with the full GUI toolkit

=back

Calling these functions will create a stand-alone window with the plot that
you want to view and return control back to the script immediately. In this
way, you can generate a series of plots with a single script, each in their
own window. In other words, you can create interactive plots while
programming with the standard procedural mindset. This also gives you a
means for learning about C<PDL::Graphics::Prima> without having to code a
large set of boilerplate.

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

=head1 SIMPLEST FUNCTIONS

These three functions are bare-bones means for visualizing your data
that are no more than simple wrappers around the more powerful
C<plot> function. If you just want to have a quick look at your data, you
should probably start with these three. See C<plot> if you need more control
over your plot, such as plotting multiple data sets, using multiple plot
types, controlling the axis scaling, or setting the axis labels.

=over

=item line_plot ($x, $y)

The C<line_plot> function takes two arguments, your x-data and your
y-data, and plots them by drawing black lines on a white background from one
point to the next. Usually C<$x> and C<$y> will have the same dimensions,
but you can use any data that are PDL-thread compatible. For example, here's
a way to compare three sets of data that have the exact same x-vaues:

 my $x = sequence(100)/10;
 my $y = sequence(3)->transpose + sin($x);
 
 # Add mild linear trends to the first and second:
 use PDL::NiceSlice;
 $y(:, 0) += $x/5;
 $y(:, 1) -= $x/6;
 
 line_plot($x, $y);

Bad values in your data, if they exist, will simply be skipped, inserting a
gap into the line.

=cut

sub line_plot {
	croak("line_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => \@_);
}

=item blob_plot ($x, $y)

Like the C<line_plot> function discussed above, the C<blob_plot> function
takes two arguments, your x-data and your y-data. It then plots black dots
against a white background. See the Synopsis for a simple example, and see
the C<line_plot> example for how to plot multidimensional data.

Bad values in your data are simply omitted from the plot.

=cut

sub blob_plot {
	croak("blob_plot expects two piddles, a set of x-coordinates and a set of y-coordinates")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Blobs]);
}

=item hist_plot ($x, $y)

The C<hist_plot> function takes two arguments, the centers of the histogram
bins and their heights. It plots the histogram as black-outined rectangles
against a white background. As with the other functions, C<$x> and C<$y>
must be thread-compatible.

Any heights or positions that are bad values are skipped, leaving a gap in
the histogram.

=cut

sub hist_plot {
	croak("hist_plot expects two piddles, the bin-centers and the bin heights")
		unless @_ == 2 and eval{$_[0]->isa('PDL') and $_[1]->isa('PDL')};
	plot(-data => [@_, plotType => pt::Histogram]);
}

=item matrix_plot ([$xbounds, $ybounds,] $matrix)

Matrix plot takes either one or three arguments. The first designates the
x-bounds of the plot; the second designates the y-bounds of the plot, and
the third specifies a matrix that you want to have plotted in grey-scale.
The x-bounds and y-bounds arguments should either be two-element arrays or
two-element piddles designating the min and the max. (You can also pass
arrays or piddles with more elements, but don't worry about that for now.)

Bad values, if your data has any, are skipped. This means that you will have
a white spot in its place (since the background is white), which is not
great. Future versions may use a different color to specify bad values.

working here - document threading

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

working here - thorough documentation needed

=cut

# A function that allows for quick one-off plots:
sub plot {
	# Make sure they sent key/value pairs:
	croak("Arguments to plot must be in key => value pairs")
		unless @_ % 2 == 0;
	my %args = @_;

	if (defined $::application) {
		# If the application is already running, then simply create a new
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
forking (as discussed below) is to create a stand-alone plot window that you
can manipulate while the rest of your script runs, and which persists
afterwards. If you want the script to block after each function cal until
the plot window closes, you can provide the C<-sequential> switch. You can
freely mix the sequential switch into your other call parameters:

 # force sequential (i.e. blocking) behavior
 use PDL::Graphics::Prima::Simple -sequential;

(If your goal is to create a script that runs B<identically> on all supported
platforms, you should always use this switch, unless I figure out how to
properly fork a seperate process under Windows.)

=cut

# import/export stuff:
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = our @EXPORT = qw(plot line_plot hist_plot blob_plot matrix_plot);

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
	if(($sequential or $^O =~ /MS/)) {
		# If this is windows, we'll use a single application that gets managed
		# by the various plot commands:
		require 'Prima.pm';
		Prima->import(qw(Application));
		require 'PDL/Graphics/Prima.pm';
		PDL::Graphics::Prima->import();
	}
}

1;

=head1 BLOCKING AND NONBLOCKING PLOTTING

Under Unix-like systems that support proper forking, namely Linux and Mac
OSX, the default behavior is to fork a process that creates the plot window
and immediately resumes the execution of your code. These windows remain
open even after your script exits, which can be handy in a number of
circumstances. It also allows for more sensible behavior with the perldl
shell, in which case making plots with these commands will give you fully
interactive plot windows and a fully working (i.e. non-blocked) shell.

When run under Windows or if you supply the C<-sequential> argument when you
C<use> this module, the plot commands will block until they are closed. This
can be helpful if you want to view a series of plots and you want to enforce
a no-clutter policy, but it does not play so well with the mental model
underlying the perldl shell. Unfortuntely, I know of no way to get
nonblocking behavior on Windows. Patches welcome!


=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2011 David Mertens. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
