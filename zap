#!/usr/bin/perl

open FILE, "ps -u novak |";

while (<FILE>) {
    $line = $_;
    foreach(@ARGV) {
	if ($line =~ /$_/) {
#	    $pid = $line;
	    @line = split(' ', $line);
	    $pid = $line[1];
#	    print "$line[1]\n";
#	    $pid =~ s/[A-z?].*//g;
#	    print $pid;
#	    chomp $pid;
	    print "Killing $pid from $line";
	    print "kill $pid\n";
#	    system "kill $pid\n";
	}
    }
}
