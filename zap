#!/usr/bin/perl

open FILE, "ps -u novak |";

while (<FILE>) {
    $line = $_;
    foreach(@ARGV) {
	if ($line =~ /$_/) {
	    $pid = $line;
	    $pid =~ s/[A-z?].*//g;
	    chomp $pid;
	    print "Killing $pid from $line";
	    print "kill $pid\n";
#	    system "kill $pid\n";
	}
    }
}
