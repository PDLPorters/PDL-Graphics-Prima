use strict;
use warnings;
use blib;
use PDL;
use PDL::NiceSlice;
use Prima qw(Application);
use PDL::Graphics::Prima;

my $N_osc = 150;
my $twopi = atan2(1,1) * 8;
my $thetas = random($N_osc);
my $omegas = grandom($N_osc) + 1;
my $speed_colors = 2**9 * ($omegas - $omegas->min) / ($omegas->max - $omegas->min);
my $dt = 2**-5;
my $K = 1.8;
my $timer;
my $timing_status = '';

my $main_window = Prima::MainWindow-> create(
	text    => 'Kuramoto Simulator',
	accelItems => [
		  ['', '', kb::Up, sub {$K += 0.1}]
		, ['', '', kb::Down, sub {$K -= 0.1}]
		, ['', '', kb::PageUp, sub {$dt *= 2}]
		, ['', '', kb::PageDown, sub {$dt /= 2}]
		, ['', '', 'q', sub {$_[0]->close}]
		, ['', '', kb::Space, sub {$timer->get_active ? $timer->stop : $timer->start}]
	],
	onPaint => sub {
		my $self = shift;
		# Calculate the coordinates of the center and the size of the radius
		my ($x_max, $y_max) = $self->size;
		my $radius = 0.4 * $x_max;
		$radius = 0.4 * $y_max if $y_max < $x_max;
		
		# Compute the theta positions:
		my $xs = cos($thetas) * $radius + $x_max/2;
		my $ys = sin($thetas) * $radius + $y_max/2;

		# Update the canvas:
		$self->backColor(cl::White);
		$self->clear;
		
		# Draw a circular track:
		$self->color(cl::Black);
		$self->ellipse($x_max/2, $y_max/2, 2*$radius, 2*$radius);
		
		# Draw all the oscillators:
		$self->pdl_fill_ellipses($xs, $ys, 15, 15, colors => $speed_colors
											, backColors => $speed_colors);
		
		# Give the latest coupling:
		$self->color(cl::Black);
		$self->text_out("K = $K; $timing_status", 0, 0);
	}
);

use Time::HiRes qw(gettimeofday tv_interval);
my $previous_time = [gettimeofday];
my $current_time;
my $timer_counter = 0;
my $update_duration = 1.5;

$timer = Prima::Timer-> create(
	timeout => 1000 * $dt, # milliseconds
	onTick  => sub {
		# Compute the lag about ever 1.5 seconds:
		$timer_counter++;
		if ($timer_counter >= $update_duration / $dt) {
			$current_time = [gettimeofday];
			my $expected_duration = $dt * $timer_counter;
			$timing_status = "Last $timer_counter steps should have taken "
				. "$expected_duration seconds; actually took "
				. tv_interval($previous_time, $current_time);
			$previous_time = $current_time;
			$timer_counter = 0;
		}
		
		# Compute r:
		my $rx = $thetas->cos->average;
		my $ry = $thetas->sin->average;
		my $r = sqrt($rx*$rx + $ry*$ry);
		my $psi = atan2($ry, $rx);
		
		# Compute the next time step:
		$thetas += $dt * ($omegas + $r * $K * sin($psi - $thetas));
		$main_window->notify('Paint');
	},
);

$timer-> start;

run Prima;

