use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple -sequential;

my $x = sequence(100)/10 + 0.1;
my $y = sin($x);
plot(
	-data => ds::Pair($x, sin $x),
	title => 'Sine Wave',
	onKeyUp => sub {
		print "You pressed a key!\n";
		print "Got args [", join('], [', @_), "]\n";
	},
);

plot(
	-data => ds::Pair($x, cos $x),
	title => 'Cosine Wave',
	x => {
		scaling => sc::Log,
	},
	onKeyUp => sub {
		print "You pressed a key!\n";
		print "Got args [", join('], [', @_), "]\n";
	},
);
