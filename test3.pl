use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;

my $t = sequence(300) / 25 + 1;
my $y = exp($t);

my $wDisplay = Prima::MainWindow->create(
	text    => 'Graph Test',
	size	=> [300, 300],
);

$wDisplay->insert('PDL::Graphics::Prima', 
	# dataSet => [$t, \&PDL::exp],
	dataSet => [$t, $y],
	pack => { fill => 'both', expand => 1},
	y => {scaling => sc::Log},
);

run Prima;
