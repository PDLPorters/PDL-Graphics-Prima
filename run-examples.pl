# A script that runs through the specified file and looks for pod sections
# marked =head(1|2) Examples, and runs them.

use strict;
use warnings;

die "You must supply a filename\n" unless @ARGV > 0;
my $filename = shift;
die "File [$filename] does not exist\n" unless -f $filename;

my $start_at = shift // 0;

# Open the filename and rip through it until we find the Examples section:
open my $fh, '<', $filename;
while(<$fh>) {
	last if /^=head\d Examples/;
}

if (eof $fh) {
	print "Didn't find any examples to run\n";
	exit;
}

# Extract the text from each example, save it in a temporary file, and execute
# it.
my $script_name = 'example.pl';
my $script_fh;
my $example_number = 0;
while(<$fh>) {
	# We're done if we find 
	last if /^=/;
	
	# If we encounter a space, print it to the filehandle:
	if (/^ +/) {
		if (not defined $script_fh) {
			open ($script_fh, '>', $script_name);
			print $script_fh "use blib;\n";
			$example_number++;
		}
		print $script_fh $_;
	}
	elsif (defined $script_fh) {
		# If we have an open filehandle, close it and execute.
		close $script_fh;
		$script_fh = undef;
		
		if ($example_number >= $start_at) {
			system('perl', $script_name);
			print "+++ Presse Enter to continue +++\n";
			<>;
		}
	}
	
	# No matter what, print out the text shown.
	print;
}
