package PDL::Graphics::Prima::ReadLine;
use strict;
use warnings;

my $is_setup = 0;

sub import {
	my ($class, $readline_obj) = @_;
	
	return if $is_setup;
	
	# Make sure we have a readline object:
	croak("PDL::Graphics::Prima::ReadLine expects a readline object")
		if not eval{ $readline_obj->isa('Term::ReadLine') };
	
	# Make sure we have a sufficiently high version of ReadLine.
	croak("PDL::Graphics::Prima::ReadLine requires Term::ReadLine v 1.09 or higher")
		if $Term::ReadLine::VERSION < 1.09;
	
	# Weird, we must call the import method for this to work, even though
	# I don't need any functions imported.
	require Prima::Application;
	Prima::Application->import;
	
	# This io watcher will (eventually) watch whatever the readline is
	# monitoring. That will be established later in the call to event_loop
	my $keep_running_event_loop = 1;
	my $prima_io_watcher = Prima::File->new(
		onRead => sub { $keep_running_event_loop = 0 },
	);
	
	$readline_obj->event_loop( sub {
			# Run the event loop. If a key is pressed, the io watcher's
			# callback will get called, changing the value of
			# $keep_running_event_loop to zero and breaking us out of this
			# while loop.
			Prima::Application::yield() while $keep_running_event_loop;
			# Having broken out of the loop, reset so we'll enter it the
			# next time this gets called.
			$keep_running_event_loop = 1;
		},
		sub {
			# Register the event loop, which means associating the io
			# watcher with the specific io handle the readline wants
			my $fh = shift;
			$prima_io_watcher->file($fh);
		},
	);
	
	$is_setup = 1;
}

# Status method
sub is_setup { $is_setup }

1;

__END__

=head1 NAME

PDL::Graphics::Prima::ReadLine - allowing Prima and Term::ReadLine to play
together

=head1 SYNOPSIS

This module's primary functionality is invoked when you use either of these
two modules from within your pdl shell:

 use PDL::Graphics::Prima;

or 

 use PDL::Graphics::Prima::Simple;

If you are not using this from within the pdl shell and need to supply your
own readline object, you can manually invoke the module and supply it as one
of the arguments like so:

 BEGIN {
   ... get $readline object ...
 }
 use PDL::Graphics::Prima::ReadLine ($readline_obj);

If you don't want to fiddle with a C<BEGIN> block, you can say this:

 ... get $readline_obj ...
 require PDL::Graphics::Prima::ReadLine;
 PDL::Graphics::Prima::ReadLine->import($readline_obj);

=head1 DESCRIPTION

Recent versions of L<Term::ReadLine> (v1.09 and higher) add support for
arbitrary event loops. This means that it is possible to add the Prima event
loop to the readline, letting you simultaneously interact with a plot and
type commands into the terminal.

This module is idempotent, which means you can invoke it multiple times
without any trouble. The first invocation of the class's C<import> method is
guaranteed to either set up the event loop or die.

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

=item L<PDL::Graphics::Prima::ReadLine>

Encapsulates all interaction with the L<Term::ReadLine> family of
modules.

=item L<PDL::Graphics::Prima::Scaling>

Specifies different kinds of scaling, including linear and logarithmic

=item L<PDL::Graphics::Prima::Simple>

Defines a number of useful functions for generating simple and not-so-simple
plots

=back

=head1 LICENSE AND COPYRIGHT

This module's code are copyright (c) 2013 Northwestern University.

This module's documentation are copyright (c) 2013 David Mertens.

All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
