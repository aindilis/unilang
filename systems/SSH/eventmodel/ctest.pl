#!/usr/bin/perl -w

use Event qw(loop unloop);
use Term::ReadKey;

ReadMode('cbreak');
Event->timer(interval=>0.1, cb=>\&KeyStrokeTest);
my $ret = loop();

sub KeyStrokeTest {
  if (defined ($char = ReadKey(-1)) ) {
    print "<<<$char>>>\n";
  } else {
    # no input was waiting
  }
}
