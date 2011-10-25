use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple;

my $x = sequence(100)/10;
my $y = sin($x);
plot(-data => [$x, $y],
	onKeyUp => sub {
		print "You clicked the mouse\n";
		print "Got args [", join('], [', @_), "]\n";
	},
);