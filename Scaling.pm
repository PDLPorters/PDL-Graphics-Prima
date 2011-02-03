use strict;
use warnings;

# Defines the scaling classes.

package sc;
use constant Linear => 'Prima::Ex::Graph::Scaling::Linear';
use constant Log => 'Prima::Ex::Graph::Scaling::Log';

package Prima::Ex::Graph::Scaling::Linear;

use PDL;


our $offset_penalty = 2;

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


sub compute_ticks {
	my (undef, $min, $max) = @_;
	# This algorithm was designed with the following major tick layout
	# examples in mind:
	#	min  	max		ticks at
	#	2		4		2, 2.5, 3, 3.5, 4
	#	2.2		4.3		2.25, 2.75, 3.25, 3.75, 4.25 (maybe)
	#					2.4, 2.8, 3.2, 3.6, 4.0 (maybe)
	#	0		20		0, 5, 10, 15, 20
	#	30.5	37.5	31, 33, 35, 37
	#	0		112		0, 25, 50, 75, 100
	
	# Determine the order of magnitude of the min/max range:
	my $full_range = $max - $min;
	# Start by assuming that the order of magnitude will be a good
	# step size:
	my $exponent = log($max - $min) / log(10);
	# Correct for tracation of negative numbers:
	$exponent-- if $exponent < 0;
	my $order_of_magnitude = 10**(int $exponent);
	# Score each potential interval and take the lowest one:
	my $best_score = 100;
	my $best_Ticks;
	my $best_interval;
	my $best_scaled_interval;
	my $best_offset;
	
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
			
			# Count the number of ticks between min_Tick and max_Tick
			my $N_Ticks = ($max_Tick - $min_Tick) / $interval + 1;
			
			# Obviously, max tick and min tick must not overlap:
			next if $N_Ticks < 2;
			
			# compute the score:
			$handicap += $offset_penalty if $offset != 0;
			my $score = (abs($N_Ticks - 5) + $handicap)**2;
			
			# If it's the best score, use it:
			if ($score < $best_score) {
				$best_score = $score;
				# Add 0.1 to N_Ticks to avoid rounding dilemas:
				$best_Ticks = zeroes($N_Ticks + 0.1)->xlinvals($min_Tick, $max_Tick);
				$best_interval = $interval;
				$best_scaled_interval = $scaled_interval;
				$best_offset = $offset;
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
	elsif (($best_scaled_interval eq '0.25'
			or $best_scaled_interval eq '0.5'
			or $best_scaled_interval eq '2.5'
			) and $best_offset == 0
	) {
		$tick_interval = $best_interval / 5;
	}
	else {
		$tick_interval = $best_interval / 4;
	}
	my $min_tick = _adjusted_position($min, $tick_interval, $best_offset, 1);
	my $max_tick = _adjusted_position($max, $tick_interval, $best_offset, -1);
	my $N_ticks = ($max_tick - $min_tick) / $tick_interval + 1.1;
	# Add 0.1 to escape rounding trouble.
	my $ticks = zeroes($N_ticks)->xlinvals($min_tick, $max_tick);

	# finally, set up zero values that make sense:
	# working here - convoluted so the debugger wouldn't get angry at me:
#	$best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min)) .= 0;
#	$ticks->where(abs($ticks) * 1e10 < ($max - $min)) .= 0;
	my $foo = $best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min));
	$foo .= 0;
	$foo = $ticks->where(abs($ticks) * 1e10 < ($max - $min));
	$foo .= 0;
	return ($best_Ticks, $ticks);
}

# Rescales the data so that the min has a value of zero and the max has a value
# of 1.
use Carp 'cluck';
sub transform {
	my ($class, $min, $max, $data) = @_;
	my $range = $max - $min;
	return (($data - $min) / $range);
}

# Rescales data so that a value of zero gets the minimum and a value of 1 gets
# the maximum:
sub inv_transform {
	my ($class, $min, $max, $data) = @_;
	my $range = $max - $min;
	return $range * $data + $min;
}

# Generates a collection of points that extrapolate evenly from the min to the
# max
sub sample_evenly {
	my ($class, $min, $max, $N_values) = @_;
	return zeroes($N_values)->xlinvals($min, $max);
}

sub is_valid_extremum { return 1 }

package Prima::Ex::Graph::Scaling::Log;

# This algorithm was based almost entirely off the linear ticks
# algorithm, with a few special tweaks.
sub compute_ticks {
	# These are the LOGARITHMS of the actual data's min and max:
	my ($min, $max) = @_;
	
	die "Not yet implemented";

=pod	

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
					0.3			0
					0.5			0
					1			0
					2			0.25
					2.5			0
					3			0.5
					4			0.25
		);
	
	while (my ($scaled_interval, $handicap) = each %intervals) {
		my $interval = $order_of_magnitude * $scaled_interval;
		# The ticks will start at either a multiple of the interval,
		# or a multiple of the interval plus the offset. So,
		# recompute the range in light of the tick size:
		my $min_Tick = _adjusted_log_position($min, $interval, 1);
		my $max_Tick = _adjusted_log_position($max, $interval, -1);
		
		# Obviously, max tick and min tick must not overlap:
		next if $max_Tick <= $min_Tick;
		
		# Count the number of ticks between min_Tick and max_Tick
		# and compute the score:
		my $N_Ticks = ($max_Tick - $min_Tick) / $interval + 1;
		$handicap += $offset_penalty if $offset != 0;
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
	
	# Construct the minor tick marks:
	my $tick_interval;
	if ($best_scaled_interval eq '0.3'
			or $best_scaled_interval eq '3'
			or $best_scaled_interval eq '0.6'
	) {
		$tick_interval = $best_interval / 6;
	}
	elsif (($best_scaled_interval eq '0.25'
			or $best_scaled_interval eq '0.5'
			or $best_scaled_interval eq '2.5'
			) and $best_offset == 0
	) {
		$tick_interval = $best_interval / 5;
	}
	else {
		$tick_interval = $best_interval / 4;
	}
	my $min_tick = _adjusted_position($min, $tick_interval, $best_offset, 1);
	my $max_tick = _adjusted_position($max, $tick_interval, $best_offset, -1);
	my $N_ticks = ($max_tick - $min_tick) / $tick_interval + 1.1;
	# Add 0.1 to escape rounding trouble.
	my $ticks = zeroes($N_ticks)->xlinvals($min_tick, $max_tick);

	# finally, set up zero values that make sense:
	$best_Ticks->where(abs($best_Ticks) * 1e10 < ($max - $min)) .= 0;
	$ticks->where(abs($ticks) * 1e10 < ($max - $min)) .= 0;
	return ($best_Ticks, $ticks);

=cut

}

# Rescales the data so that the min has a value of zero and the max has a value
# of 1.
sub transform {
	my ($class, $min, $max, $data);
	my ($log_min, $log_max, $log_data) = (log($min), log($max), log($data));
	my $range = $log_max - $log_min;
	return (($log_data - $log_min) / $range);
}

# Rescales data so that a value of zero gets the minimum and a value of 1 gets
# the maximum:
sub inv_transform {
	# working here - make sure this works
	my ($class, $min, $max, $data);
	my ($log_min, $log_max) = (log($min), log($max));
	my $range = $log_max - $log_min;
	return exp($range * $data + $log_min);
}

# Generates a collection of points that extrapolate evenly from the min to the
# max
sub sample_evenly {
	my ($class, $min, $max, $N_values) = @_;
	return zeroes($N_values)->xlogvals($min, $max);
}

sub is_valid_extremum {
	my ($class, $extremum) = @_;
	return 1 if $extremum > 0;
	# Set the error message:
	$@ = 'must be positive for logarithmic scaling';
	# Return a false value:
	return 0;
}
1;
