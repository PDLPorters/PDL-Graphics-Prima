# This was written based on some guidelines from:
# http://sourceforge.net/apps/mediawiki/pdl/index.php?title=How_to_call_C_functions_from_PDL

use PDL::Core::Dev;
use ExtUtils::MakeMaker;
use Prima::Config;

my $pack = [qw(pdlprima.pd Prima PDL::Graphics::Prima)];
%hash = pdlpp_stdargs($pack);

$hash{VERSION_FROM}="pdlprima.pd";
$hash{INC} .= join( ' ' , map { $Prima::Config::Config{cincflag}.$_} @{$Prima::Config::Config{incpaths}});
my %DEFINES=map { my @z = split('=', $_, 2); scalar(@z)?(@z):($_ => 1)} @{$Prima::Config::Config{cdefs}};
$hash{CCFLAGS} .= join( ' ' , map { $Prima::Config::Config{cdefflag}.$_.'='.$DEFINES{$_}} keys %DEFINES);

# Make it install my module files:
$hash{PM} = {
	  'Prima.pm'	=> 'blib/lib/PDL/Graphics/Prima.pm'
	, 'Graph.pm'	=> 'blib/lib/Prima/Ex/Graph.pm'
	, 'DataSet.pm'	=> 'blib/lib/Prima/Ex/Graph/DataSet.pm'
	, 'Limits.pm'	=> 'blib/lib/Prima/Ex/Graph/Limits.pm'
	, 'Scaling.pm'	=> 'blib/lib/Prima/Ex/Graph/Scaling.pm'
	, 'Axis.pm'		=> 'blib/lib/Prima/Ex/Graph/Axis.pm'
	, 'PlotType.pm'	=> 'blib/lib/Prima/Ex/Graph/PlotType.pm'
};

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble($pack);
}
