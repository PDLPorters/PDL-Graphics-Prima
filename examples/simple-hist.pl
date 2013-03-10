use strict;
use warnings;
use PDL;
use PDL::Graphics::Prima::Simple;

my $data = grandom(100);

auto_twiddle(0);

hist_plot($data);
hist_plot($data, bt::Log);
hist_plot($data, bt::Linear(min => 0, drop_extremes => 0));
hist_plot(ones(20));
hist_plot(ones(20), bt::Log(normalize => 0));

twiddle;