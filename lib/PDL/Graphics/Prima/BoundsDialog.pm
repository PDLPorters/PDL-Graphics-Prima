#################
# Bounds Editor #
#################

###################################################
#  -(x axis)------------------------------------  #
# | Label: [.......]                            | #
# | Lower Bound: (*) Auto  () Explicit: [.....] | #
# | Upper Bound: (*) Auto  () Explicit: [.....] | #
#  ---------------------------------------------  #
#  -(y axis)------------------------------------  #
# | Label: [.......]                            | #
# | Lower Bound: (*) Auto  () Explicit: [.....] | #
# | Upper Bound: (*) Auto  () Explicit: [.....] | #
#  ---------------------------------------------- #
# [] Auto-apply             [OK] [Apply] [Cancel] #
###################################################

package PDL::Graphics::Prima::BoundsDialog;

use Prima qw(Application Buttons InputLine);

# A 5 pixel buffer:
my $buf = 5;
my $current_plot_widget;
my $auto_apply = 0;
my $bounds_dialog = Prima::Window->create( size => [ 400, 400 ]);

# ---( Basic Containers )--- #
my $x_group = $bounds_dialog->insert(GroupBox =>
	text => 'x Axis',
	pack => {
		expand => 1, fill => 'x', side => 'top',
		padx => $buf, pady => $buf,
	},
);
my $y_group = $bounds_dialog->insert(GroupBox =>
	text => 'y Axis',
	pack => {
		expand => 1, fill => 'x', side => 'top',
		padx => $buf, pady => $buf,
	},
);
my $bottom_container = $bounds_dialog->insert(Widget => 
	pack => {
		expand => 1, fill => 'both', side => 'top',
		padx => $buf, pady => $buf,
	},
);

# ---( x-widgets )--- #
# protect the group box's description text:
$x_group->insert(Widget => height => 0,
	pack => { anchor => 'w', fill => 'x', side => 'top', pady => 25 });
# label edit line:
my $x_label_container = $x_group->insert(Widget => 
	pack => { padx => 20, expand => 1, fill => 'x', side => 'top' });
$x_label_container->insert(Label =>	text => 'Label: ',	pack => { side => 'left'});
$x_label_container->insert(InputLine =>
	onKeyUp => sub {
		if ($auto_apply) {
			my $self = shift;
			$current_plot_widget->x->title($self->value);
		}
	},
	pack => {side => 'left', expand => 1, fill => 'x'}
);
# lower-bound edit line:
my $x_lower_bound_container = $x_group->insert(Widget => 
	pack => { expand => 1, fill => 'x', side => 'top', padx => 20 });
$x_lower_bound_container->insert(Label => text => 'Lower Bound:', pack => { side => 'left'});
my ($x_lower_bound_auto, $x_lower_bound_explicit, $x_lower_bound_value);
$x_lower_bound_auto = $x_lower_bound_container->insert(Radio =>
	text => 'Auto',
	onCheck => sub {
		return unless $_[1] == 1;
		$x_lower_bound_explicit->uncheck;
		$current_plot_widget->x->min(lm::Auto) if $auto_apply;
		print "checked lower-bound auto Radio button\n";
	},
	pack => { padx => 10, anchor => 'w', side => 'left' },
);
$x_lower_bound_explicit = $x_lower_bound_container->insert(Radio =>
	text => 'Explicit:',
	onCheck => sub {
		return unless $_[1] == 1;
		$x_lower_bound_auto->uncheck;
		print "checked lower-bound explicit Radio button\n";
	},
	pack => { padx => 10, anchor => 'w', side => 'left' },
);
$x_lower_bound_value = $x_lower_bound_container->insert(InputLine =>
	text => 0.0,
	onKeyUp => sub {
		print "Validating lower-bound value\n";
	},
	pack => { padx => 10, anchor => 'w', fill => 'x', side => 'left' },
);
# upper-bound edit line:
my $x_upper_bound_container = $x_group->insert(Widget => 
	pack => { expand => 1, fill => 'x', side => 'top', padx => 20 });
$x_upper_bound_container->insert(Label => text => 'Upper Bound:', pack => { side => 'left'});
$x_upper_bound_container->insert(Radio =>
	text => 'Auto',
	onEnter => sub {
		print "selected upper-bound auto Radio button\n";
	},
	pack => { padx => 10, anchor => 'w', fill => 'x', side => 'left' },
);
# protect the group box's bottom edge
$x_group->insert(Widget => height => 0,
	pack => { anchor => 'w', fill => 'x', side => 'top', pady => 2 });


$bounds_dialog->onEndModal(sub {
	
});

# ---( Bottom Container )--- #


sub run {
	$current_plot_widget = shift;
	# working here:
	# $x_lower_bound_value->value($current_plot_widget->x->min);
	$bounds_dialog->execute;
}

1;