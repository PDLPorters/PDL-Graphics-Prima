use strict;
use warnings;

# Propogates the version string in lib/PDL/Graphics/Prima.pm to the version
# strings in the other modules.

open my $in_fh, '<', 'lib/PDL/Graphics/Prima.pm';
# Find the version string
while(<$in_fh>) {
	last if /^our \$VERSION = (\d\.\d\d.{4})/;
}
my $version = $1;
close $in_fh;

# OK, now run through all the module files
for my $module_filename (glob 'lib/PDL/Graphics/Prima/*.pm') {
	next if $module_filename =~ /Internals/;
	print "Updating $module_filename\n";
	open my $out_fh, '+<', $module_filename;
	while (my $line = <$out_fh>) {
		if ($line =~ /\$VERSION = /) {
			print $out_fh "our \$VERSION = $1\n";
			last;
		}
	}
	close $out_fh;
}
