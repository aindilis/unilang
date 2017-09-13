#!/usr/bin/perl -w

use PerlLib::SwissArmyKnife;
use Time::HiRes qw ( time );

my $time = time();
my $localtime = localtime();

print Dumper
  ({
    Time => $time,
    Localtime => $localtime,
   });



