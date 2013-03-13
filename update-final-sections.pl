use strict;
use warnings;

# Propogates everything after and including the AUTHOR section in
# lib/PDL/Graphics/Prima.pm to the concluding sections of the other modules.

open my $in_fh, '<', 'lib/PDL/Graphics/Prima.pm';
# Skip until we find the SEE ALSO section
while (my $line = <$in_fh>) {
	last if $line =~ /AUTHOR/;
}

# Pull the rest in
my @concluding_lines = <$in_fh>;

# OK, now run through all the module files
for my $module_filename (glob 'lib/PDL/Graphics/Prima/*.pm') {
	next if $module_filename =~ /Interactive/;
	print "Updating $module_filename\n";
	open my $out_fh, '+<', $module_filename;
	while (my $line = <$out_fh>) {
		last if $line =~ /AUTHOR/;
	}
	# Add these contents and truncate the rest
	print $out_fh @concluding_lines;
	truncate $out_fh, tell($out_fh);
	close $out_fh;
}
