#!/usr/bin/perl -w

use Data::Dumper;
use DateTime;
use IO::Socket;
use IO::Select;
use Time::HiRes qw( usleep );

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

my $PORT = 9000;
my $server = IO::Socket::INET->new
  (
   Proto     => 'tcp',
   LocalPort => $PORT,
   Listen    => SOMAXCONN,
   Reuse     => 1,
  );

my $readset = IO::Select->new;
$readset->add($server);

while (1) {
  usleep(5000);
  my @handles = $readset->can_read(0);
  print Dumper(\@handles) if scalar @handles;
  foreach my $handle (@handles) {
    if ($handle eq $server) {
      my $handle = $server->accept();
      $readset->add($handle);
      print Dumper($handle);
    } else {
      if ($handle->connected) {
	while (defined($_ = $handle->getline())) {
	  my $dt = DateTime->now;
	  print $dt->hms."<$_>\n";
	}
      }
    }
  }
}

1;
