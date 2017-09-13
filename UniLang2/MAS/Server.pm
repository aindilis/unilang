package UniLang2::MAS::Server;

use UniLang2::MAS::Client;
use UniLang2::Util::Authenticator;
use UniLang2::Util::Message;
use UniLang2::Util::System;
use PerlLib::SwissArmyKnife;

use Event;
use Linux::Inotify2;

# use IO::Socket;
# use IO::Socket::SSL;
# use IO::Select;
# use IO::Handle;
# use Net::hostent;
# use Net::Telnet;

use Time::HiRes qw( usleep );

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker new_with_init => 'new',
  get_set =>
  [

   qw /

	MAS Host Port Server Clients ReadSet NewClientNameCounter SSL
	Delay BadMessageCounter Debug Buffers


	MyINotify Name MessageDir IncomingDir AgentIncomingFiles
	ClientMessageCounters

      /

  ];

sub init {
  my ($self,%args) = @_;
  # V2

  $self->Host($args{Host} || "localhost");
  $self->Port($args{Port} || "9000");

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


  $self->Name($args{Name} || $self->Port);
  $self->AgentIncomingFiles({});
  $self->MessageDir("/var/lib/myfrdcsa/codebases/internal/unilang/data/messages");
  system "rm -rf /var/lib/myfrdcsa/codebases/internal/unilang/data/messages/servers/*";
  system "rm -rf /var/lib/myfrdcsa/codebases/internal/unilang/data/messages/clients/*";

  $self->ClientMessageCounters({});
  $self->MyINotify
    (
     Linux::Inotify2->new() or
     die "Unable to create new inotify object: $!"
    );
  Event->io
    (
     fd => $self->MyINotify->fileno,
     poll => 'r',
     cb => sub { $self->MyINotify->poll },
    );
  $self->IncomingDir(ConcatDir($self->MessageDir,"servers",$self->Name,"incoming"));
  MkDirIfNotExists
    (
     Directory => $self->IncomingDir,
    );
  $self->MyINotify->watch
    (
     $self->IncomingDir,
     IN_ALL_EVENTS,
     sub {
       my $e = shift;
       my $name = $e->fullname;
       if ($e->IN_MODIFY) {
	 system "cat $name > /dev/null";
	 my $raw = read_file($name);
	 print Dumper({
		       Raw => $raw,
		       Name => $name,
		      }) if 0;
	 my $file = basename($name);
	 if ($file =~ /^(.+)-(\d+)$/) {
	   my $clientname = $1;
	   if (! exists $self->Clients->{$clientname}) {
	     $self->SetupClient
	       (Client => UniLang2::MAS::Client->new
		(Name => $clientname));
	   }
	   my $message = UniLang2::Util::Message->new
	     (
	      Raw => $raw,
	     );
	   $self->MAS->ProcessCommunicationEvent
	     (Message => $message,
	      Client => $self->Clients->{$clientname});
	   # system "rm ".shell_quote($name);
	 }
       }
     },
    );

  die "can't setup server" unless $self->MyINotify;

  $self->Debug($args{Debug});

  $self->MAS($args{MAS});
  $self->Clients({});
  $self->NewClientNameCounter(0);

  print "[Server $0 accepting clients]\n";
}

# sub Listen {
#   my ($self,%args) = @_;
#   # V2
#   my $count = 0;
#   my $majorcount = 0;
#   while (1) {
#     ++$count;
#     if ($count > 100) {
#       $self->PerformChecks(Type => "minor");
#       $count = 0;
#       ++$majorcount;
#     }
#     if ($majorcount > 100) {
#       $self->PerformChecks(Type => "major");
#       $majorcount = 0;
#     }
#     my $timeout = 0;
#     # usleep($self->Delay);
#     $self->MyINotify->poll;
#   }
# }

sub Listen {
  my ($self,%args) = @_;
  if (exists $args{TimeOut}) {
    eval {
      local $SIG{ALRM} = sub { die "alarm\n" }; # NB: \n required
      alarm $args{TimeOut};
      $self->MyINotify->poll;
      alarm 0;
    };
    if ($@) {
      print "Error with Listen\n" unless $@ eq "alarm\n";   # propagate unexpected errors
      # timed out
    }
    else {
      # didn't
    }
  } else {
    $self->MyINotify->poll;
  }
  # $self->PIDFile->remove if $self->PIDFile;
}

sub SetupClient {
  my ($self,%args) = @_;
  # V2
  my $client = $args{Client};
  my $clients = $self->Clients;
  if (exists $clients->{$client->Name}) {
    print "Client by the name of $client->Name already exists\n";
  } else {
    MkDirIfNotExists
      (Directory => ConcatDir($self->MessageDir,"clients",$args{Client}->Name,"incoming"));
    $clients->{$client->Name} = $client;
    # now make a new agent for it
    $self->ClientMessageCounters->{$client->Name} = 0;

    $client->Agent
      (UniLang2::MAS::Agent->new
       (Name => $client->Name,
	Client => $client));
  }
  $self->Clients($clients);
}

# sub ProcessMessages {
#   my ($self,%args) = @_;
#   # the idea here is to create a queue of the contents from various
#   # buffers, and to process these for messages, and then processing
#   # those messages
#   my $name = $args{Client}->Agent->Name;
#   if (! defined $self->Buffers->{$name}) {
#     $self->Buffers->{$name} = [];
#   }
#   my $aref = $self->Buffers->{$name};
#   my $handle = $args{Client}->Handle;
#   $handle->blocking(0);
#   while (defined($_ = $handle->getline())) {
#     print Dumper($_) if $self->Debug;
#     push @$aref, $_;
#   }
#   # okay, now we're done reading, so we can process these messages
#   my @lines;
#   while (scalar @$aref) {
#     my $line = shift @$aref;
#     push @lines, $line;
#     if ($line =~ /^<\/message>/) {
#       my $contents = join("",@lines);
#       print Dumper($contents) if $self->Debug;
#       my $message = UniLang2::Util::Message->new
# 	(Raw => $contents);
#       print Dumper($message) if $self->Debug;
#       @lines = ();
#     }
#   }
#   push @$aref, @lines;
# }

# sub ProcessMessage {
#   my ($self,%args) = @_;
#   my $contents = $self->Receive(Handle => $args{Client}->Handle);
#   # print Dumper($contents);
#   my $message;
#   if ($contents) {
#     $message = UniLang2::Util::Message->new
#       (Raw => $contents);
#     $self->MAS->ProcessCommunicationEvent
#       (Message => $message,
#        Client => $args{Client});
#     # print Dumper($contents);
#     # verify that client is authorized to send a message from Agent listed in sender
#     # verify that client is authorized to send a message to Agent lister in receiver
#     return 1;
#   }
# }

sub Send {
  my ($self,%args) = @_;
  # V2
  my $fh = IO::File->new();
  my $filename = ConcatDir($self->MessageDir,"clients",$args{ClientName},"incoming",$self->Name."-".$self->ClientMessageCounters->{$args{ClientName}});
  my $message = $args{Message};
  my $logmessage = uc($args{LogMessage}) || "SENDING";
  if ($self->Debug) {
    print $logmessage." ".$message->Sender." to ".$message->Receiver.
      (exists $message->Data->{_TransactionID} ? " transactionid ".$message->Data->{_TransactionID}. " transactionsequence ".$message->Data->{_TransactionSequence} : "")."\n";
    # print join(",",sort keys %{$message->Data})."\n\n";
    print Dumper($message);
  }
  print Dumper({Filename => $filename}) if 0;
  $fh->open(">$filename") or warn "Cannot open file\n";
  print $fh   $args{Message}->Generate;
  $fh->close();
  $self->ClientMessageCounters->{$args{ClientName}} = $self->ClientMessageCounters->{$args{ClientName}} + 1;
}

# sub Receive {
#   my ($self,%args) = @_;
#   my $handle = $args{Handle};
#   my $line;
#   my $contents;
#   $line = $handle->getline();
#   if ($line) {
#     $contents .= $line;
#     my $closure = $line;
#     $closure =~ s/^</<\//;
#     do {
#       $line = $handle->getline();
#       $contents .= $line;
#     } while ($line ne $closure);
#     return $contents;
#   }
#   return;
# }

sub ListClients {
  my ($self,%args) = @_;
  values %{$self->Clients};
}

# sub GetClientFromHandle {
#   my ($self,$handle) = (shift,shift);
#   # look up a client using its handle
#   foreach my $client ($self->ListClients) {
#     if ($client->Handle eq $handle) {
#       return $client;
#     }
#   }
#   return;
# }

sub NewClientName {
  my ($self,%args) = @_;
  "client-".$self->NewClientNameCounter($self->NewClientNameCounter + 1);
}

sub PerformChecks {
  my ($self,%args) = @_;
  # deregister any agents that are still at large
  #   foreach my $agent (values %{$self->MAS->Agents}) {
  #     if ($args{Type} eq "major") {			# only do this every once in a while, like 5 minutes
  #       # print "Performing ping checks\n";
  #       my $message = UniLang2::Util::Message->new
  # 	(
  # 	 Sender => "UniLang",
  # 	 Receiver => $agent->Name,
  # 	 Contents => "",
  # 	 Data => {
  # 		  _Ping => 1,
  # 		  _DoNotLog => 1,
  # 		 },
  # 	);
  #       $self->Send
  # 	(Message => $message,
  # 	 Handle => $agent->Client->Handle);
  #     }
  #     if (! $agent->Client->Handle->connected) {
  #       # lookup which agent this socket belongs to and deregister the
  #       # agent...
  #       $self->MAS->DeRegister(Agent => $agent->Name);
  #     }
  #   }
}

sub IsConnected {
  my ($self,%args) = @_;
  my $dirname = $self->GetClientDirFromClientName(ClientName => $args{ClientName});
  return -d $dirname;
}

sub RemoveClient {
  my ($self,%args) = @_;
  if ($self->IsConnected(ClientName => $args{ClientName})) {
    my $dirname = $self->GetClientDirFromClientName(ClientName => $args{ClientName});
    system "mv ".shell_quote($dirname)." /tmp";
  }
}

sub GetClientDirFromClientName {
  my ($self,%args) = @_;
  return ConcatDir($self->MessageDir,"clients",$args{ClientName});
}

1;
