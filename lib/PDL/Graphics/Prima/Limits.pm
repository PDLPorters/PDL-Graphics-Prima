use strict;
use warnings;

# Defines the lm (limits) package:
package lm;
my $inf;
BEGIN {
	use PDL::Lite;
	$inf = -PDL->pdl(0)->log->at(0);
}

use constant Auto => $inf;
use constant Hold => -$inf;

1;

=head1 NAME

PDL::Graphics::Prima::Limits - defining a couple of useful constants

=head1 DESCRIPTION

You probably won't ever need to use this module explicitly, but you will likely
use the constants defined here to manipulate axis autoscaling. This module defines
the constants C<lm::Auto> and C<lm::Hold>. If the explanation below does not
make sense, these constants are also discussed in
L<PDL::Graphics::Prima::Axis>, L<PDL::Graphics::Prima::Simple>, and elsewhere.

=over

=item lm::Auto

When you set an axis's min or max to C<lm::Auto>, you turn on min or max
autoscaling:

 # Set the x-min to -5 for now...
 $plot->x->min(-5);
 
 # Turn on autoscaling for x-min:
 $plot->x->min(lm::Auto);

=item lm::Hold

This constant gives a shorthand for changing from autoscaling to non-autoscaling.
For example, suppose you are building a plot from multiple data sets and want
to autoscale based on the first few but not for the remaining. In that case you
might say:

 $plot->dataSets->{'data'} = ds::Pair($x, $y);
 $plot->y->minmax(lm::Hold, lm::Hold);
 $plot->dataSets->{'model'} = ds::Func(\&my_func);

You can achieve the same ends like so:

 $plot->dataSets->{'data'} = ds::Pair($x, $y);
 $plot->y->minmax($plot->y->minmax);
 $plot->dataSets->{'model'} = ds::Func(\&my_func);

If you just wanted to set the min to hold, you could use C<lm::Hold> like this:

 $plot->y->min(lm::Hold);

which is equivalent to:

 $plot->y->min($plot->y->min);

Also note that the return value of C<< $plot->y->min >> returns different things
depending on whether you are using scalar or list context. (Yes, that's an Axis
thing, not a Limits thing, but it bears repeating here anyway.)

=back

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

Portions of this module's code are copyright (c) 2011 The Board of Trustees at
the University of Illinois.

This module's documentation are copyright (c) 2011-2012 David Mertens.

All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
