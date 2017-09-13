#!/usr/bin/perl -w

use Linux::Inotify2;

# create a new object
my $inotify = new Linux::Inotify2
  or die "Unable to create new inotify object: $!" ;

# create watch
$inotify->watch ("test", IN_ACCESS)
  or die "watch creation failed" ;

while () {
  my @events = $inotify->read;
  unless (@events > 0) {
    print "read error: $!";
    last ;
  }
  printf "mask\t%d\n", $_->mask foreach @events ;
}
