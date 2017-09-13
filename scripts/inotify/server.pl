#!/usr/bin/perl -w

use INotifyServer;
use PerlLib::SwissArmyKnife;

my $server = INotifyServer->new;

$server->StartListening
  (
   ReceiveHandler => sub {PrintMessage(Args => \@_)},
  );

sub PrintMessage {
  my %args = @_;
  print Dumper($args{Args});
}
