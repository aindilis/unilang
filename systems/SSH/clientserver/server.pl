#!/usr/bin/perl -w

use IO::Socket;
use IO::Select;
use IO::Handle;
use Net::hostent;
use Net::Telnet;
use UniLang::Util::Message;

use strict;
use Carp;

my $PORT = 9000;
my $server = IO::Socket::INET->new( Proto     => 'tcp',
				    LocalPort => $PORT,
				    Listen    => SOMAXCONN,
				    Reuse     => 1);
die "can't setup server" unless $server;

my $handle = $server->accept();

while (1) {
  my $message = UniLang::Util::Message->new(Raw => receive());
  print $message->Generate;
}

sub receive {
  my $line;
  my $contents;
  $line = $handle->getline();
  $contents .= $line;
  my $closure = $line;
  $closure =~ s/^</<\//;
  do {
    $line = $handle->getline();
    $contents .= $line;
  } while ($line ne $closure);
  return $contents;
}
