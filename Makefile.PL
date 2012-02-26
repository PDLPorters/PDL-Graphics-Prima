# A simple MakeMaker script

# Should require whichever PDL has whereND
use ExtUtils::MakeMaker;
WriteMakefile(
	NAME => 'PDL::Graphics::Prima',
	VERSION_FROM => 'lib/PDL/Graphics/Prima.pm',
);

