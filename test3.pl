use strict;
use warnings;
use PDL;

sub _adjusted_position {
	my ($value, $interval, $offset, $direction) = @_;
	# Find the first copy of $interval that's less than $value:
	my $to_return = int($value / $interval) * $interval;
	# include the offset:
	$to_return += $offset;
	while(1) {
		return $to_return if $value == $to_return;
		return $to_return if (($to_return <=> $value) == $direction);
		$to_return += $direction * $interval;
	}
	# Return the final, adjusted position:
	return $to_return;
}

sub compute_linear_ticks {
	my ($min, $max) = @_;
	
	# This algorithm is designed to compute the following ticks:
	#	min  	max		ticks at
	#	2		4		2, 2.5, 3, 3.5, 4
	#	2.2		4.3		2.5, 3, 3.5, 4
	#					2.25, 2.75, 3.25, 3.75, 4.25
	#					2.4, 2.8, 3.2, 3.6, 4.0
	#	0		20		0, 5, 10, 15, 20
	#	30.5	37.5	31, 33, 35, 37
	#	0		112		0, 25, 50, 75, 100
	
	# Determine the order of magnitude of the min/max range:
	my $full_range = $max - $min;
	# Start by assuming that the order of magnitude will be a good
	# step size:
	my $order_of_magnitude = 10**(int (log($max - $min) / log(10)));
	
	# Score each potential interval and take the lowest one:
	my $best_score = 100;
	my $best_Ticks;
	my $best_interval;
	my $best_scaled_interval;
	
	my %intervals # interval	# handicap
		= qw(		0.25		0
					0.3			0.5
					0.4			0.25
					0.5			0
					0.6			0.5
					0.8			0.25
					1			0
					2			0.25
					2.5			0
					3			0.5
					4			0.25
		);
	
	while (my ($scaled_interval, $handicap) = each %intervals) {
		my $interval = $order_of_magnitude * $scaled_interval;
		for my $offset (0, $interval/2) {
			# The ticks will start at either a multiple of the interval,
			# or a multiple of the interval plus the offset. So,
			# recompute the range in light of the tick size:
			my $min_Tick = _adjusted_position($min, $interval, $offset, 1);
			my $max_Tick = _adjusted_position($max, $interval, $offset, -1);
			
			# Obviously, max tick and min tick must not overlap:
			next if $max_Tick <= $min_Tick;
			
			# Count the number of ticks between min_Tick and max_Tick
			# and compute the score:
			my $N_Ticks = ($max_Tick - $min_Tick) / $interval + 1;
			$handicap += 0.1 if $offset != 0;
			my $score = (abs($N_Ticks - 5) + $handicap)**2;
			
			# If it's the best score, use it:
			if ($score < $best_score) {
				$best_score = $score;
				# Add 0.1 to N_Ticks to avoid rounding dilemas:
				$best_Ticks = zeroes($N_Ticks + 0.1)->xlinvals($min_Tick, $max_Tick);
				$best_interval = $interval;
				$best_scaled_interval = $scaled_interval;
			}
		}
	}
	
	# Construct the minor tick marks:
	my $tick_interval;
	if ($best_scaled_interval eq '0.3'
			or $best_scaled_interval eq '3'
			or $best_scaled_interval eq '0.6'
	) {
		$tick_interval = $best_interval / 6;
	}
	elsif ($best_scaled_interval eq '0.25'
			or $best_scaled_interval eq '0.5'
			or $best_scaled_interval eq '2.5'
	) {
		$tick_interval = $best_interval / 5;
	}
	elsif ($best_scaled_interval eq '0.4'
			or $best_scaled_interval eq '0.8'
			or $best_scaled_interval eq '1'
			or $best_scaled_interval eq '2'
			or $best_scaled_interval eq '4'
	) {
		$tick_interval = $best_interval / 4;
	}
	my $min_tick = _adjusted_position($min, $tick_interval, 0, 1);
	my $max_tick = _adjusted_position($max, $tick_interval, 0, -1);
	my $N_ticks = ($max_tick - $min_tick) / $tick_interval + 1.1;
	# Add 0.1 to escape rounding trouble.
	my $ticks = zeroes($N_ticks)->xlinvals($min_tick, $max_tick);

	# finally, set up zero values that make sense:
	$best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min)) .= 0;
	$ticks->where(abs($ticks) * 1e10 < ($max - $min)) .= 0;
	return ($best_Ticks, $ticks);
}

#for (1..10) {
	my $left = 2.2;#rand(1000);
	my $right = 4.3;#rand(1000);
	($left, $right) = ($right, $left) if ($left > $right);

	my ($Ticks, $ticks) = compute_linear_ticks($left, $right);
	print "For $left to $right, got Ticks of\n$Ticks\nand ticks of\n$ticks\n";
#}
