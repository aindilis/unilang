#!/usr/bin/perl -w

use KBS::Util;

my $item = shift;

print PerlDataDedumperToStringEmacs
  (String => $item);
