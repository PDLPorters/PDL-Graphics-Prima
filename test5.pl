use blib;
 use strict;
 use warnings;
 use PDL;
 use Prima qw(Application);
 use Prima::Graph::Simple;
 use PDL::FFT;
 use PDL::NiceSlice;
 
 my $dt = 1/50;
 my $t = sequence(31400) * $dt;
 my $y = sin($t);
 my $fft = $y->copy;
 my $df = 1 / $dt / $t->nelem;
# my $freq = $y->sequence * $df;
 realfft($fft);
	my $N_steps = $t->nelem;

#	my $power = rotate($fft(0:$N_steps/2-1)**2
#					+ $fft($N_steps/2:-1)**2, $N_steps/4);
#	my $freq = $power->xlinvals(-($N_steps/2 - 1) / $N_steps / $dt / 2, 1/4 / $dt);
    my $power = $fft(0:$N_steps/2-1)**2 + $fft($N_steps/2:)**2;
    my $freq = $power->sequence / $N_steps / $dt;
 
 
my $window = Prima::MainWindow-> new( 
        text => 'Spectrum Explorer',
        size => [ 600, 300],
);

my $plot = $window->insert( Widget =>
	backColor => cl::White,
	geometry => gt::Place,
	placeInfo => {x => 0, y => 0, relwidth => 0.5, relheight => 1
			, anchor => 'sw'},
);
Prima::Graph::Simple::endow($plot, $t, $y, 'time', 'sin', 'Sine curve');

my $spectrum = $window->insert( Widget =>
	backColor => cl::Blue,
	geometry => gt::Place,
	placeInfo => {relx => 0.5, y => 0, relwidth => 0.5, relheight => 1
			, anchor => 'sw'},
);
Prima::Graph::Simple::endow($spectrum, $freq, $power, 'Freq (Hz)', 'Spectrum', 'Spectrum');

run Prima;
