#!/usr/bin/perl -w

use UniLang::Agent::Agent;
use UniLang::Util::Message;

use Data::Dumper;

my $search = shift;
my $name = shift;

my $agent = UniLang::Agent::Agent->new
  (Name => $name || "test-agent",
   ReceiveHandler => \&Receive);

sub Receive {
 my %args = @_;
  $command = $args{Message}->Contents;
  ProcessCommand($command);
  $agent->Deregister;
}

sub Start {
  $agent->Register(Host => "localhost",
		 Port => "9000");
  $agent->SendContents
    (Receiver => "Sorcerer",
     Contents => "-a $search");
  # $agent->Listen;
}

Start;

sub ProcessCommand {
  my $c = @_;
  print Dumper($c);
}



