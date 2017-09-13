#!/usr/bin/perl -w

use Event qw(loop unloop);
Event->io(fd=>\*STDIN, cb=>\&ReceiveMessage);
my $ret = loop();
sub ReceiveMessage {
  my $it = <>;
  chomp $it;
  print "<<<$it>>>\n";
}
