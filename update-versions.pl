use strict;
use warnings;

# Propogates the version string in lib/PDL/Graphics/Prima.pm to the version
# strings in the other modules.

open my $in_fh, '<', 'lib/PDL/Graphics/Prima.pm';
# Find the version string
my $version;
while(<$in_fh>) {
	if (/^our \$VERSION = (\d\.\d\d.{4})/) {
		$version = $1;
		last;
	}
}
defined $version or die "Unable to extract the version!!!\n";
close $in_fh;

# OK, now run through all the module files
for my $module_filename (glob 'lib/PDL/Graphics/Prima/*.pm') {
	next if $module_filename =~ /Internals/;
	open my $out_fh, '+<', $module_filename;
	while (my $line = <$out_fh>) {
		if ($line =~ /\$VERSION = (.{8})/) {
			if ($1 ne $version) {
				print "Updating $module_filename\n";
				# reposition to the start of the line
				seek $out_fh, -length($line), 1;
				print $out_fh "our \$VERSION = $version";
			}
			last;
		}
	}
	close $out_fh;
}
