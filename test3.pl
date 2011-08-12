use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;

my $t_data = sequence(6) / 0.5 + 1;
my $y_data = exp($t_data);

my $wDisplay = Prima::MainWindow->create(
	text    => 'Graph Test',
	size	=> [300, 300],
);

$wDisplay->insert('Plot',
	-function => [\&PDL::exp, color => cl::Blue],
	-data => [$t_data, $y_data, color => cl::Red],
	pack => { fill => 'both', expand => 1},
#	y => {scaling => sc::Log},
);

run Prima;

