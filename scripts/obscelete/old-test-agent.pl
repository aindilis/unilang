#!/usr/bin/perl -w

use UniLang::Agent::Agent;
use UniLang::Util::Message;

use Data::Dumper;

my $search = shift;
my $name = shift;
my $agent = UniLang::Agent::Agent->new
  (Name => $name || "test-agent",
   ReceiveHandler => \&Receive);

sub Start {
  $agent->Register(Host => "localhost",
		 Port => "9000");
  print $agent->QueryAgent
    (Agent => "Formalize",
     Contents => "This is a test sentence.");
}

sub ProcessCommand {
  my $c = @_;
  print Dumper($c);
}

sub Receive {
 my %args = @_;
  $command = $args{Message}->Contents;
  ProcessCommand($command);
}

Start;
$agent->Deregister;


