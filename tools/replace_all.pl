#!/usr/bin/perl
use strict;
use warnings;
use 5.010;

print "reading tokens\n";
open my $tokenh, '<', '/tmp/nuke_tokens' or die "Could not open filename: $!\n";

print "reading PICO source\n";
open my $sourceh, '<', '../carts/nuke_fmt.p8' or die "Can't read file filename: $!\n";
my $source = do { local $/; <$sourceh> };
close $sourceh;

my %tokens;
while(my $line = <$tokenh>)  {   
	my @fields = split /;/, $line;
	$fields[1] =~ s/[\n\r]//g;
	$tokens{@fields[0]} = $fields[1];
}
close $tokenh;

my $i = 0;
my %idtokenpairs;
print "source tokens:\n";
foreach my $it (keys %tokens) {
	print "${it}\n";
	$source =~ s/$it/__${i}__/g;
	# find minified token
	$idtokenpairs{"__${i}__"}= $tokens{$it};
	$i=$i+1;
}

# fix for bogus label minification
$source =~ s/::die::/::ho::/g;
open(my $desth, '>', '../carts/nuke_mini.tmp') or die "Could not open file filename: $!";
print $desth $source;
close $desth;

foreach (keys %idtokenpairs) {
	my $val=$idtokenpairs{$_};
	print "replacing: $_ -> $val\n";
	$source =~ s/$_/$val/g;
}

open(my $desth, '>', '../carts/nuke_mini.p8') or die "Could not open file filename: $!";
print $desth $source;
close $desth;
