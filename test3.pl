use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use Prima::Ex::Graph;

my $t = sequence(300) / 25;
my $y = exp($t);

my $wDisplay = Prima::MainWindow->create(
	text    => 'Graph Test',
	size	=> [300, 300],
);

$wDisplay->insert('Prima::Ex::Graph', 
	dataSet => [\&PDL::exp],
	pack => { fill => 'both', expand => 1},
	x => {scaling => sc::Log},
);

run Prima;
