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

You probably won't ever need to use this module explicitly. It defines the
constants C<lm::Auto> and C<lm::Hold>, which are described in
L<PDL::Graphics::Prima::Axis>, L<PDL::Graphics::Prima::Simple>, and elsewhere.

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
