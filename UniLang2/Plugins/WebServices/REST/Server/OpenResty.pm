package UniLang2::Plugins::WebServices::REST::Server::OpenResty;

use base qw(UniLang2::Plugins::WebServices::REST::Server);

use WWW::OpenResty;

use UniLang2::Util::Message;

use Data::Dumper;

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw /  /

  ];

sub init {
  my ($self, %args) = @_;
  $self->Name($args{Name});

}

sub Register {
  my ($self, %args) = @_;
  $self->Connect(%args);
  $self->Startup;
  $self->Daemonize;
}

sub Deregister {
  my ($self, %args) = @_;
  $self->Shutdown;
  $self->Disconnect;
}

sub Startup {
  my ($self, %args) = @_;
  # just create a message to send to unilang2 with your name
  $self->SendContents
    (Contents => "Register");
}

sub Shutdown {
  my ($self, %args) = @_;
  # just send a message to unilang2 with saying you are quiting
  if (defined $self->Client) {
    $self->SendContents
      (Contents => "Deregister");
  }
}

sub Connect {
  my ($self, %args) = @_;
  my $client;
  if ($self->SSL) {
    if (!($client = IO::Socket::SSL->new( PeerAddr => $args{Host} || "localhost",
    					  PeerPort => $args{Port} || "9000",
    					  Proto    => 'tcp',
    					  SSL_use_cert => 1,
    					  SSL_verify_mode => 0x01,
    					  SSL_passwd_cb => sub { return "opossum" },
    					))) {
      warn "unable to create socket: ", &IO::Socket::SSL::errstr, "\n";
      exit(0);
    } else {
      warn "connect ($client).\n" if ($IO::Socket::SSL::DEBUG);
    }
    # check server cert.
    my ($subject_name, $issuer_name, $cipher);
    if ( ref($client) eq "IO::Socket::SSL") {
      $subject_name = $client->peer_certificate("subject");
      $issuer_name = $client->peer_certificate("issuer");
      $cipher = $client->get_cipher();
      warn "cipher: $cipher.\n", "server cert:\n", 
    	"\t '$subject_name' \n\t '$issuer_name'.\n\n";
    }
    #     $client = Net::SSL->new
    #       (PeerAddr => $args{Host} || "localhost",
    #        PeerPort => $args{Port} || "9000",
    #        Timeout => 3);
  } else {
    my $f = File::Temp->new;
    $client = Net::Telnet->new
      (%args,
       Dump_Log => $f->filename);
    my $msg = 0;
    while (! $client->open) {
      print "Cannot open connection to server.  Retrying periodically.\n" unless $msg;
      $msg = 1;
      sleep 10;
    }
  }
  $client->autoflush(0);
  $self->Client($client);

  # probably want to add something here about reconnecting
  # periodically, so that if a write fails, it waits and waits and
  # waits or something like that.
  # $self->Client->errmode(sub {});
  #   my $msg = 0;
  #   while (! $self->Client->open) {
  #     print "Cannot open connection to server.  Retrying periodically.\n" unless $msg;
  #     $msg = 1;
  #     sleep 10;
  #   }
  my $handle = $self->Client;
  Event->io
    (fd => \*$handle,
     timeout => 3600,
     cb => sub { $self->ClientHandler });
}

sub Disconnect {
  my ($self, %args) = @_;
  if (defined $self->Client) {
    $self->Client->close;
  }
}

sub OneEvent {
  my ($self,%args) = @_;
  # one_event($args{TimeOut});
}

sub Listen {
  my ($self,%args) = @_;
  if (exists $args{TimeOut}) {
    loop($args{TimeOut});
  } else {
    loop();
  }
  # $self->PIDFile->remove if $self->PIDFile;
}

sub UnListen {
  my ($self,%args) = @_;
  my $ret = unloop();
}

sub GetDate {
  my ($self,%args) = @_;
  chomp (my $date = `date`);
  return $date;
}

sub MySTDINHandler {
  my ($self,%args) = @_;
  my $contents = <STDIN>;
  if (defined $contents and $contents =~ /./) {
    chomp $contents;
    $self->SendContents
      (Contents => $contents);
  } else {
    usleep(10000);
  }
}

sub ClientHandler {
  my ($self,%args) = @_;
  my $contents = $self->Receive(Handle => $self->Client);
  if ($contents) {
    my $message = UniLang2::Util::Message->new(Raw => $contents);
    if ($message) {
      if (exists $message->Data->{_Ping}) {
	# this is a ping, ignore it
      } else {
	if ($self->ReceiveHandler) {
	  &{$self->ReceiveHandler}(Message => $message);
	} else {
	  print $message->Generate;
	  #print "Message from $message->{sender}:\n";
	  #print $message->Contents."\n";
	}
      }
    }
  }
}

sub Print {
  my ($self,%args) = @_;
  print $self->Name . "\n";
}

sub Send {
  my ($self,%args) = @_;
  my $message = $args{Message};
  my $handle = $args{Handle};
  if (defined $handle) {
    printflush $handle $message->Generate;
  }
}

sub SendContents {
  my ($self,%args) = @_;
  my $message = UniLang2::Util::Message->new
     (
      Sender => $args{Sender} || $self->Name,
      Receiver => $args{Receiver} || "UniLang",
      Date => $args{Date},
      Contents => $args{Contents},
      Data => $args{Data},
     );
  # print Dumper($message);
  $self->Send
    (
     Handle => $self->Client,
     Message => $message,
    );
}

sub Receive {
  my ($self,%args) = @_;
  my $handle = $args{Handle};
  my $line;
  my $contents;
  # $line = $handle->getline(Timeout => 3600);
  $line = $handle->getline();
  if ($line) {
    $contents .= $line;
    my $closure = $line;
    $closure =~ s/^\</\<\//;
    do {
      $line = $handle->getline();
      $contents .= $line;
    } while ($line ne $closure);
    return $contents;
  }
  return;
}

sub Reply {
  my ($self,%args) = @_;
  $self->SendContents
    (
     Receiver => $args{Message}->Sender,
     Contents => $args{Contents},
     Data => $args{Data},
    );
}

sub GetTransactionID {
  return rand();
}

sub QueryAgent {
  my ($self,%args) = @_;
  # initialize everything
  my $bf = $self->BufferMessage;
  $self->BufferMessage(undef);
  my $rh = $self->ReceiveHandler;
  $self->ReceiveHandler
    (sub {$self->ReceiveToBufferMessage(@_)});
  # send and receive
  # print Dumper(\%args);
  $args{Data}->{_TransactionID} = $self->GetTransactionID;
  $args{Data}->{_TransactionSequence} = 0;
  $self->SendContents
    (%args);
  $self->Listen();
  # reset everything and return results
  $self->ReceiveHandler($rh);
  my $message = $self->BufferMessage;
  $self->BufferMessage($bf);
  if (defined $message) {
    return $message;
  }
}

sub QueryAgentReply {
  my ($self,%args) = @_;
  if (defined $args{Message}->Data) {
    if (exists $args{Message}->Data->{_TransactionID}) {
      $args{Data}->{_TransactionID} = $args{Message}->Data->{_TransactionID};
      $args{Data}->{_TransactionSequence} = $args{Message}->Data->{_TransactionSequence} + 1;
    }
  }
  $args{Receiver} ||= $args{Message}->Sender;
  delete $args{Message};
  $self->SendContents
    (
     %args
    );
}

sub Restart {
  my ($self,%args) = @_;
  $self->Deregister;
  $self->Register
    (Host => "localhost",
     Port => "9000");
}

sub ReceiveToBuffer {
  my ($self,%args) = @_;
  $self->Buffer($args{Message}->Contents);
  $self->UnListen;
}

sub ReceiveToBufferMessage {
  my ($self,%args) = @_;
  $self->BufferMessage($args{Message});
  $self->UnListen;
}

sub ChangeName {
  my ($self,%args) = @_;
  $self->Shutdown;
  $self->Name($args{Name});
  $self->Startup;
}

sub Ping {
  my ($self,%args) = @_;
  return 1;
}

1;
