use blib;
 use strict;
 use warnings;
 use PDL;
 use PDL::Graphics::Prima;
 use Prima qw(Application);
 
 my $window = Prima::MainWindow->create(
     text    => 'PDL::Graphics::Prima Test',
     onPaint => sub {
         my ( $self, $canvas) = @_;
 
         # wipe and replot:
         $canvas->clear;
         
         ### Example code goes here ###
 
 $canvas->fill_poly([0, 0, 49, 200, 943, 123, 92, 3]);
 
# my ($x_max, $y_max) = $canvas->size;
# 
# my $xs = zeroes($N_arcs)->random * $x_max;
# my $ys = $xs->random * $y_max;
# my $dxs = $xs->random * $x_max / 4;
# my $dys = $xs->random * $y_max / 4;
# my $th_starts = $xs->random * 360;
# my $th_stops = $xs->random * 360;
# 
# # Now that we've generated the data, call the command:
# $canvas->pdl_arcs($xs, $ys, $dxs
#                , $dys, $th_starts, $th_stops);
         
     },
     backColor => cl::White,
 );
 
 run Prima;
