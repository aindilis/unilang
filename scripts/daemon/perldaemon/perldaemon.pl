#!/usr/bin/perl -w

BEGIN {
 # Fork.
 my $pidFile = '/var/run/perldaemon.pid';
 my $pid = fork;
 if ($pid) # parent: save PID
 {
  open PIDFILE, ">$pidFile" or die "can't open $pidFile: $!\n";
  print PIDFILE $pid;
  close PIDFILE;
  exit 0;
 }
}

while (1) {};
