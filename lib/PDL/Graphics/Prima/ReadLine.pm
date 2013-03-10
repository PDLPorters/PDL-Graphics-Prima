package PDL::Graphics::Prima::ReadLine;
use strict;
use warnings;
use Carp;

my $is_setup = 0;

sub is_happy_with {
	my ($class, $readline_obj) = @_;
	return eval{ $readline_obj->can('event_loop')
		and not $readline_obj->tkRunning };
}

sub setup {
	my ($class, $readline_obj) = @_;
	
	return if $is_setup;
	
	# Make sure we have a readline object that knows how to work with the
	# event loop:
	if (not $class->is_happy_with($readline_obj)) {
		croak("PDL::Graphics::Prima::ReadLine expects a readline object")
			unless eval { $readline_obj->isa('Term::ReadLine::Stub') };
		croak("PDL::Graphics::Prima::ReadLine expects a readline object that knows how to event_loop.\n"
				. "This is provided in Term::ReadLine v1.09 or newer");
	}
	
	# Weird, we must call the import method for this to work, even though
	# I don't need any functions imported (because we call yield by its fully
	# qualified name).
	require Prima::Application;
	Prima::Application->import;
	
	# This io watcher will (eventually) watch whatever the readline is
	# monitoring. That will be established later in the call to event_loop.
	# Die'ing is a simple way to exit the "go" method invoked during the
	# event loop
	my $prima_io_watcher = Prima::File->new(
		onRead => sub { die 'user pressed a key' },
	);
	
	$readline_obj->event_loop( sub {
			local $@;
			# Run the event loop. If a key is pressed, the io watcher's
			# callback will get called, throwing an exception.
			eval { $::application->go };
			# Rethrow the exception if it's not one that we threw
			die unless $@ =~ /user pressed a key/;
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
