package UniLang::MAS::Server;

use UniLang::MAS::Client;
use UniLang::Util::Authenticator;
use UniLang::Util::Message;
use UniLang::Util::System;

use Data::Dumper;
use IO::Socket;
use IO::Socket::SSL;
use IO::Select;
use IO::Handle;
use Net::hostent;
use Net::Telnet;
use Time::HiRes qw( time usleep );

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker new_with_init => 'new',
  get_set =>
  [

   qw /

	MAS Host Port Server Clients ReadSet NewClientNameCounter SSL
	Delay BadMessageCounter Debug Buffers ShutdownSeen

      /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Host($args{Host} || "localhost");
  $self->Port($args{Port} || "9000");
  my $server;
  my $bogomips = 0;
  my $delay;
  foreach my $line (split /\n/, `cat /proc/cpuinfo`) {
    if ($line =~ /^bogomips\s+:\s+(.+)$/) {
      $bogomips += $1;
    }
  }
  if ($bogomips > 0) {
    # 10000 usleep for 3000 bogomips, inversly proportional
    $delay = int((5000.0 * 3000) / $bogomips);
  } else {
    $delay = 5000;
  }
  $self->Delay($delay);
  $self->BadMessageCounter({});
  $self->Buffers({});
  $self->SSL(0);
  if ($self->SSL) {
    if (!($server = IO::Socket::SSL->new( Listen => 5,
					  LocalAddr => 'localhost',
					  LocalPort => ,
					  Proto     => 'tcp',
					  Reuse     => 1,
					  SSL_verify_mode => 0x01,
					  SSL_passwd_cb => sub {return "bluebell"},
					)) ) {
      warn "unable to create socket: ", &IO::Socket::SSL::errstr, "\n";
      exit(0);
    }
    warn "socket created: $server.\n";

  } else {

    $server = IO::Socket::INET->new
      (
       Proto     => 'tcp',
       LocalPort => $self->Port,
       Listen    => SOMAXCONN,
       Reuse     => 1,
      );
  }
  die "can't setup server" unless $server;
  if (exists $args{Conf}->{'-t'}) {
    print "Server started OK\n";
    exit(0);
  }

  # $self->Debug(1);

  $self->MAS($args{MAS});
  $self->Server($server);
  $self->Clients({});
  $self->ReadSet(IO::Select->new);
  $self->NewClientNameCounter(0);

  print "[Server $0 accepting clients]\n";
  $self->ReadSet->add($self->Server);
}

sub Listen {
  my ($self,%args) = @_;
  my $count = 0;
  my $majorcount = 0;
  while (1) {
    ++$count;
    if ($count > 100) {
      $self->PerformChecks(Type => "minor");
      $count = 0;
      ++$majorcount;
    }
    if ($majorcount > 100) {
      # $self->PerformChecks(Type => "major");
      $majorcount = 0;
    }
    my $timeout = 0;
    usleep($self->Delay);
    my @handles = $self->ReadSet->can_read($timeout);
    print Dumper(\@handles) if scalar @handles and $self->Debug;
    foreach my $handle (@handles) {
      if ($handle eq $self->Server) {
	my $handle = $self->Server->accept();

	# SSL server stuff
	if ( ! $handle ) {
	  warn "error: ", $self->Server->errstr, "\n";
	  next;
	}
	my ($subject_name,$issuer_name);
	if ( ref($self->Server) eq "IO::Socket::SSL") {
	  warn "connection opened ($handle).\n";
	  $subject_name = $handle->peer_certificate("subject");
	  $issuer_name = $handle->peer_certificate("issuer");
	  warn "\t subject: '$subject_name'.\n";
	  warn "\t issuer: '$issuer_name'.\n";
	}
	# end SSL server stuff

	$self->SetupClient
	  (Client => UniLang::MAS::Client->new
	   (Name => $self->NewClientName,
	    Handle => $handle));
      } else {
	if ($handle->connected) {
	  my $result;
	  my $client = $self->GetClientFromHandle($handle);
	  my $name = $client->Agent->Name;
	  print Dumper($self->BadMessageCounter->{$name}) if $self->Debug;
	  print Dumper($client) if $self->Debug;
	  if (exists $self->BadMessageCounter->{$name} and $self->BadMessageCounter->{$name} > 200) {
	    $self->TroubleshootClient
	      (Client => $client);
	  }
	  $self->ProcessMessages
	    (Client => $client);
	}
      }
    }
  }
}

sub TroubleshootClient {
  my ($self,%args) = @_;
  my $client = $args{Client};
  my $line;
  print "TROUBLESHOOTING\n";
  $self->ReadSet->remove($client->Handle);
  while (defined($line = $client->Handle->getline())) {
    print "<$line>\n";
  }
}

sub SetupClient {
  my ($self,%args) = @_;
  my $client = $args{Client};
  my $clients = $self->Clients;
  if (exists $clients->{$client->Name}) {
    print "Client by the name of $client->Name already exists\n";
  } else {
    $clients->{$client->Name} = $client;
    my $handle = $client->Handle;
    $self->ReadSet->add($handle);
    # now make a new agent for it
    $client->Agent
      (UniLang::MAS::Agent->new
       (Name => $client->Name,
	Client => $client));
  }
  $self->Clients($clients);
}

sub ProcessMessages {
  my ($self,%args) = @_;
  # the idea here is to create a queue of the contents from various
  # buffers, and to process these for messages, and then processing
  # those messages
  my $name = $args{Client}->Agent->Name;
  if (! defined $self->Buffers->{$name}) {
    $self->Buffers->{$name} = [];
  }
  my $aref = $self->Buffers->{$name};
  my $handle = $args{Client}->Handle;
  $handle->blocking(0);
  while (defined($_ = $handle->getline())) {
    print Dumper($_) if $self->Debug;
    push @$aref, $_;
  }
  # okay, now we're done reading, so we can process these messages
  my @lines;
  while (scalar @$aref) {
    my $line = shift @$aref;
    push @lines, $line;
    if ($line =~ /^<\/message>/) {
      my $contents = join("",@lines);
      print Dumper($contents) if $self->Debug;
      my $message = UniLang::Util::Message->new
	(Raw => $contents);
      print Dumper($message) if $self->Debug;
      @lines = ();
      $self->MAS->ProcessCommunicationEvent
	(Message => $message,
	 Client => $args{Client});
    }
  }
  push @$aref, @lines;
}

sub ProcessMessage {
  my ($self,%args) = @_;
  my $contents = $self->Receive(Handle => $args{Client}->Handle);
  # print Dumper($contents);
  my $message;
  if ($contents) {
    $message = UniLang::Util::Message->new
      (Raw => $contents);
    $self->MAS->ProcessCommunicationEvent
      (Message => $message,
       Client => $args{Client});
    # print Dumper($contents);
    # verify that client is authorized to send a message from Agent listed in sender
    # verify that client is authorized to send a message to Agent lister in receiver
    return 1;
  }
}

sub Send {
  my ($self,%args) = @_;
  my $message = $args{Message};
  my $handle = $args{Handle};
  $handle->blocking(1);
  print $handle $message->Generate;
}

sub Receive {
  my ($self,%args) = @_;
  my $handle = $args{Handle};
  my $line;
  my $contents;
  $line = $handle->getline();
  if ($line) {
    $contents .= $line;
    my $closure = $line;
    $closure =~ s/^</<\//;
    do {
      $line = $handle->getline();
      $contents .= $line;
    } while ($line ne $closure);
    return $contents;
  }
  return;
}

sub ListClients {
  my ($self,%args) = @_;
  values %{$self->Clients};
}

sub GetClientFromHandle {
  my ($self,$handle) = (shift,shift);
  # look up a client using its handle
  foreach my $client ($self->ListClients) {
    if ($client->Handle eq $handle) {
      return $client;
    }
  }
  return;
}

sub NewClientName {
  my ($self,%args) = @_;
  "client-".$self->NewClientNameCounter($self->NewClientNameCounter + 1);
}

sub PerformChecks {
  my ($self,%args) = @_;
  # deregister any agents that are still at large
  foreach my $agent (values %{$self->MAS->Agents}) {
    if ($args{Type} eq "major") { # only do this every once in a while, like 5 minutes
      # print "Performing ping checks\n";
      my $message = UniLang::Util::Message->new
	(
	 Sender => "UniLang",
	 Receiver => $agent->Name,
	 Contents => "",
	 Data => {
		  _Ping => 1,
		  _DoNotLog => 1,
		 },
	);
      $self->Send
	(Message => $message,
	 Handle => $agent->Client->Handle);
    }
    if (! $agent->Client->Handle->connected) {
      # lookup which agent this socket belongs to and deregister the
      # agent...
      $self->MAS->DeRegister(Agent => $agent->Name);
    }
    if (exists $UNIVERSAL::unilang->Config->CLIConfig->{'-W'}) {
      my $delay = $UNIVERSAL::unilang->Config->CLIConfig->{'-W'} || 1000;
      $delay = $delay / 1000.0;
      # send exit to all agents
      # skip for now
      if (time() > ($UNIVERSAL::unilang->StartTime + $delay)) {
	my $therearestillagents = 0;
	if (! defined $self->ShutdownSeen) {
	  $self->ShutdownSeen({});
	}
	foreach my $agent ($self->MAS->ListAgents) {
	  $therearestillagents = 1;
	  if (! exists $self->ShutdownSeen->{$agent->Name}) {
	    $self->MAS->SendContents
	      (
	       Sender => 'UniLang',
	       Client => $agent->Client,
	       Receiver => $agent->Name,
	       Contents => 'exit',
	       Data => {
			_DoNotLog => 1,
		       },
	      );
	    $self->ShutdownSeen->{$agent->Name} = 1;
	  }
	}
	if (! $therearestillagents) {
	  exit(0);
	}
      }
    }
  }
}

1;
