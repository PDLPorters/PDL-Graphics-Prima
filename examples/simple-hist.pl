use strict;
use warnings;
use PDL;
use PDL::Graphics::Prima::Simple;

my $data = grandom(100);

hist_plot($data->hist);
