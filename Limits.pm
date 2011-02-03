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
