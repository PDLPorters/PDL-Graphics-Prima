use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple -sequential;

my $x = sequence(100)/10;
my $y = sin($x);
plot(
	-data => [$x, sin $x],
	title => 'Sine Wave',
	onKeyUp => sub {
		print "You clicked the mouse\n";
		print "Got args [", join('], [', @_), "]\n";
	},
);

plot(
	-data => [$x, cos $x],
	title => 'Cosine Wave',
	onKeyUp => sub {
		print "You clicked the mouse\n";
		print "Got args [", join('], [', @_), "]\n";
	},
);
