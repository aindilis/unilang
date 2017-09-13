#!/usr/bin/perl -w

use Expect;

my $expect = Expect->new();
my $command = "/var/lib/myfrdcsa/codebases/internal/unilang/start -s -u localhost 9000 -c";
$expect->spawn($command, ())
  or die "Cannot spawn $command: $!\n";
print "Waiting for UniLang to initialize...\n";
# use expect to start it and wait for it to be ready
$expect->expect(300, [qr/\[Server (.+) accepting clients\]/, sub {print "UniLang is Initialized.\n"}]);
$expect->clear_accum();
# fork off and die!
my $pid;
defined($pid = fork()) or die "Cannot fork()!\n";
if (! $pid) {
  # this mean I am child?
  $expect->expect(30000, [qr/UniLang has been told to quit now/s, sub { print "Dying\n"; exit(0); } ]);
} else {
  sleep 100;
}
