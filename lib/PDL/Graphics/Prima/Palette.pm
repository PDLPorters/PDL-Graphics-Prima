use strict;
use warnings;
use PDL::Drawing::Prima::Utils;

=head1 NAME

PDL::Graphics::Prima::Palette - a set of palettes for the Prima graph widget

=head1 DESCRIPTION

Suppose you want to use color to convey some meaningful value. For example,
you want the color to represent the topography of a landscape, darker is
lower, lighter is higher. In that case, you need a mapping from a height to
a color, i.e. from a scalar value to a color. This is what palettes provide.

=cut

# At this point, I am debating just how closely I want Palettes to interact
# with Axes. I know I want to provide both linear and logarithmic scaling
# options. I would like to provide for auto-scaling as well as a means for
# explicitly stating the min and max of the color scale. I want to provide a
# means for easy display of the palette, with tick marks and labels. Then
# again, I could simply have the Palette hold its own scaling and create a
# special widget to draw the Palette (and possibly another widget to draw
# legends, but now I'm getting ahead of myself) 
#
# Let's start with the basics: the Palette knows how to scale data and
# convert scaled values to color values.

package PDL::Graphics::Prima::Palette;
use Carp;

=head2 new

You will almost always create palettes using the nick-names in the C<pal>
namespace. However, all Palette classes know how to make new copies of
themselves.

=cut

sub new {
	my $class = shift;
	croak("${class}::new expects arguments in key=>value pairs")
		unless @_ % 2 == 0;
	
	my $self = {@_};
	croak("${class}::new requires that you supply an 'apply' key with a subroutine reference")
		unless exists $self->{apply} and ref($self->{apply})
			and ref($self->{apply}) eq 'CODE';
	
	return bless $self, $class;
}

=head2 apply

Every palette knows how to apply itself to its data. The apply function
returns a piddle of Prima color values given a piddle of scalar values.

=cut

# A basic palette knows how to apply itself to a set of data. The basic
# palette invokes the subroutine reference supplied in the apply key:
sub apply {
	my ($self, $data) = shift;
	return $self->{apply}->($data);
}

=head2 plotType

Every Palette knows the specific data and plot type to which it belongs.
The first time that a Palette is used in a drawing operation, it will become
associated with that specific plotType object, which is in turn associated
with that specific dataSet and widget. Thereafter, you can retrieve the
plotType object using this accessor, but you cannot change it. If you want
to use the same Palette on a different plotType, you can copy it using
C<pal::Copy>.

=cut

sub plotType {
	my $self = shift;
	
	# See if they're requesting the plotType, return it if we have one, and
	# croak if we don't:
	if (@_ == 0) {
		return $self->{plotType} if exists $self->{plotType};
		croak("Requested palette's plot type, but it does not have one");
	}
	
	# If they are setting the plotType, croak if it has already been set,
	# since this is a write-once property
	if (exists $self->{plotType}) {
		croak("You can only set a palette's plot type once");
	}
	
	# Otherwise, they're setting the plotType.
	$self->{plotType} = $_[0];
}

=head2 copy

Not yet implemented.

You can make a copy of a Palette that does not have an associated plotType.
This way, if you put a lot of effort into making a palette, you can easily
reuse that palette with minimal effort.

=cut

sub copy {
	die "working here";
}

package PDL::Graphics::Prima::Palette::HSVrange;
our @ISA = qw(PDL::Graphics::Prima::Palette);
use Carp;

# working here - consider a constructor that knows how to deal with 
# piddles that have a width of 3?

sub new {
	my $class = shift;
	croak("${class}::new expects key=>value pairs")
		unless @_ % 2 == 0;
	
	# Default to a rainbow spectrum:
	my $self = {
		h_start => 0,
		h_stop  => 360,
		s_start => 1,
		s_stop  => 1,
		v_start => 1,
		v_stop  => 1,
		@_
	};
	return bless $self, $class;
}

sub apply {
	my ($self, $data) = @_;
	
	# Get the data and scale it from zero to one:
	my ($min, $max) = $data->minmax;
	my $scaled_data = ($data - $min) / ($max - $min);
	
	# Compute the associated hue, saturation, and vaue:
	my $h = $scaled_data * ($self->{h_stop} - $self->{h_start})
		+ $self->{h_start};
	my $s = $scaled_data * ($self->{s_stop} - $self->{s_start})
		+ $self->{s_start};
	my $v = $scaled_data * ($self->{v_stop} - $self->{v_start})
		+ $self->{v_start};
	
	return $h->cat($s, $v)->mv(-1,0)->hsv_to_rgb->rgb_to_color;
}

=head1 Special Palettes

This module provides many ready-made palettes with short-name constructors
in the C<pal> namespace.

=over

=item pal::Rainbow

Runs from red->orange->yellow->green->blue->purple->red in ascending order.

=cut

sub pal::Rainbow {
	return PDL::Graphics::Prima::Palette::HSVrange->new;
}

=item pal::BlackToWhite

Larger values are white, smaller values are black

=cut

sub pal::BlackToWhite {
	return PDL::Graphics::Prima::Palette::HSVrange->new(
		h_start => 0,
		h_stop  => 0,
		s_start => 0,
		s_stop  => 0,
		v_start => 0,
		v_stop  => 1,
	);
}

=item pal::WhiteToBlack

Larger values are black, smaller values are white

=cut

sub pal::WhiteToBlack {
	return PDL::Graphics::Prima::Palette::HSVrange->new(
		h_start => 0,
		h_stop  => 0,
		s_start => 0,
		s_stop  => 0,
		v_start => 1,
		v_stop  => 0,
	);
}

=item pal::WhiteToHSV

Smaller values are closer to white, larger values are closer to the color
indicated by the HSV values that you specify, which are supplied to the
function as three different scalars.

For example:

 my ($h, $s, $v) = pdl(cl::LightRed)->color_to_rgb->rgb_to_hsv->dog;
 my $white_to_red = pal::WhiteToHSV($h, $s, $v);

=cut

sub pal::WhiteToHSV {
	return PDL::Graphics::Prima::Palette::HSVrange->new(
		h_start => 0,
		s_start => 0,
		v_start => 1,
		h_stop  => $_[0],
		s_stop  => $_[1],
		v_stop  => $_[2],
	);
}

=item pal::BlackToHSV

Like WhiteToHSV, but smaller values are closer to black instead of white.

=cut

sub pal::BlackToHSV {
	return PDL::Graphics::Prima::Palette::HSVrange->new(
		h_start => 0,
		s_start => 0,
		v_start => 0,
		h_stop  => $_[0],
		s_stop  => $_[1],
		v_stop  => $_[2],
	);
}

=item pal::HSVrange

Goes in ascending order from the start to the stop values in hue, saturation,
and value. All values are specified with key => value pairs. For example, to
go from blue to red:

 my ($start_h, $start_s, $start_v)
   = pdl(cl::LightBlue)->color_to_rgb->rgb_to_hsv->dog;
 my ($stop_h, $stop_s, $stop_v)
   = pdl(cl::LightRed)->color_to_rgb->rgb_to_hsv->dog;
 
 my $blue_to_red = pal::HSVrange(
       h_start => $start_h,
       s_start => $start_s,
       v_start => $start_v,
       h_stop  => $stop_h,
       s_stop  => $stop_s,
       v_stop  => $stop_v,
   );

=cut

sub pal::HSVrange {
	return PDL::Graphics::Prima::Palette::HSVrange->new(@_);
}

1;

=back

=head1 AUTHOR

David Mertens (dcmertens.perl@gmail.com)

=head1 SEE ALSO

This is a component of L<PDL::Graphics::Prima>. This library is composed of many
modules, including:

=over

=item L<PDL::Graphics::Prima>

Defines the Plot widget for use in Prima applications

=item L<PDL::Graphics::Prima::Axis>

Specifies the behavior of axes (but not the scaling)

=item L<PDL::Graphics::Prima::DataSet>

Specifies the behavior of DataSets

=item L<PDL::Graphics::Prima::Internals>

A dumping ground for my partial documentation of some of the more complicated
stuff. It's not organized, so you probably shouldn't read it.

=item L<PDL::Graphics::Prima::Limits>

Defines the lm:: namespace

=item L<PDL::Graphics::Prima::Palette>

Specifies a collection of different color palettes

=item L<PDL::Graphics::Prima::PlotType>

Defines the different ways to visualize your data

=item L<PDL::Graphics::Prima::Scaling>

Specifies different kinds of scaling, including linear and logarithmic

=item L<PDL::Graphics::Prima::Simple>

Defines a number of useful functions for generating simple and not-so-simple
plots

=back

=head1 LICENSE AND COPYRIGHT

Portions of this module's code are copyright (c) 2011 The Board of Trustees at
the University of Illinois.

Portions of this module's code are copyright (c) 2011-2012 Northwestern
University.

This module's documentation are copyright (c) 2011-2012 David Mertens.

All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
