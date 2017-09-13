#!/usr/bin/perl -w

use Event;
use Linux::Inotify2;

# create a new object
my $inotify = new Linux::Inotify2
  or die "Unable to create new inotify object: $!";

# for Event:
Event->io (fd =>$inotify->fileno, poll => 'r', cb => sub { $inotify->poll });
# for Glib:
# add_watch Glib::IO $inotify->fileno, in => sub { $inotify->poll };
# manually:

# add watchers
$inotify->watch ("test", IN_ACCESS, sub {
		   my $e = shift;
		   my $name = $e->fullname;
		   print "$name was accessed\n" if $e->IN_ACCESS;
		   print "$name is no longer mounted\n" if $e->IN_UNMOUNT;
		   print "$name is gone\n" if $e->IN_IGNORED;
		   print "events for $name have been lost\n" if $e->IN_Q_OVERFLOW;

		   # cancel this watcher: remove no further events
		   $e->w->cancel;
		 });

1 while $inotify->poll;
