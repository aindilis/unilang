#!/usr/bin/perl -w

use Data::Dumper;
use utf8;

my $item = "De Luain, bhí me ag caint leat faoi ruin";
print Dumper($item);
my $item1 = $item;
utf8::encode($item);
print Dumper($item);
my $item2 = $item;
utf8::decode($item);
print Dumper($item);
my $item3 = $item;

print Dumper([$item1,$item2,$item3]);
