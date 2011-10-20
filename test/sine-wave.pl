use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple;

my $x = sequence(100)/10;
my $y = sin($x);
line_plot($x, $y);
