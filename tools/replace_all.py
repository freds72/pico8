#!/usr/bin/perl
use strict;
use warnings;

print "reading tokens\n";
open my $infoh, '<', '/tmp/nuke_tokens' or die "Could not open filename: $!\n";

print "reading PICO source\n";
open my $sourceh, '<', './nuke_fmt.p8' or die "Can't read file filename: $!\n";
my $source = do { local $/; <$sourceh> };
close $sourceh;

my @matches = ($source =~ /("[a-z_0-9]+")/g);
print "source tokens:\n";
foreach (@matches) {
	print "$_\n";
}

#while(my $line = <$infoh>)  {   
#	my @fields = split /;/, $line;
#	$fields[1] =~ s/[\n\r]//g;
#	print "replacing: $fields[0] -> $fields[1]\n";
#	$source =~ s/$fields[0]/$fields[1]/g;
#}

open(my $desth, '>', './nuke_mini.p8') or die "Could not open file filename: $!";
print $desth $source;
close $desth;

close $infoh;
