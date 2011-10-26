use strict;
use warnings;
use PDL;
use blib;
use Prima qw(Application);
use PDL::Graphics::Prima;
use PDL::NiceSlice;

my $t_series = random(101)->cumusumover;
my $heights = $t_series(1:) - $t_series(:-2);

my $wDisplay = Prima::MainWindow->create(
	text    => 'Spike Test',
	size	=> [500, 500],
);

 my $smiley_plot_type = pt::CallBack(
 	base_class => 'PDL::Graphics::Prima::PlotType::Blobs',
 	draw => sub {
 		my ($self, $dataset, $widget) = @_;
 		
 		# Retrieve the data from the dataset:
 		my ($xs, $ys) = $dataset->get_data_as_pixels($widget);
 		
 		# Draw the smileys:
 		$widget->pdl_ellipses($xs, $ys, 10, 10);	# face
 		$widget->pdl_fill_ellipses($xs - 5, $ys + 4, 2, 2);	# left eye
 		$widget->pdl_fill_ellipses($xs + 5, $ys + 4, 2, 2); # right eye
 		$widget->pdl_fill_chords($xs, $ys + 3, 10, 10, 200, 340); # smiling mouth
 	},
 	radius => 10,	# be sure to coordinate with pdl_ellipses, above
 );


$wDisplay->insert('Plot',
#	-data => [$t_series, $heights],
	-data => [$t_series(:-2), $heights, plotType => $smiley_plot_type],
	pack => { fill => 'both', expand => 1},
#	y => {scaling => sc::Log},
);

run Prima;
