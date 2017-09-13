#!/usr/bin/perl -w

use Net::Telnet;
use Net::hostent;
use strict;
use Carp;
use vars qw($VERSION);
use UniLang::Util::Message;


my $handle = Net::Telnet->new(Host => "localhost",
			   Port => "9000");

while (<>) {
  chomp;
  my $message = UniLang::Util::Message->new(Sender => "Client",
					 Receiver => "Server",
					 Date => `date`,
					 Contents => $_);
  Send($message);
}

sub Send {
  my $message = shift;
  print $handle $message->Generate;
}

1;
