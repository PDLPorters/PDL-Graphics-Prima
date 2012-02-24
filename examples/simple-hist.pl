use strict;
use warnings;
use PDL;
use blib;
use PDL::Graphics::Prima::Simple;

my $data = grandom(100);

hist_plot($data->hist);
