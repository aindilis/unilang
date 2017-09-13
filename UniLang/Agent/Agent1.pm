package UniLang::Agent::Agent;

use UniLang::Util::Message;

use Data::Dumper;
use Event qw (loop unloop); # one_event
use IO::Handle;
use IO::Socket::SSL;
use File::Pid;
use File::Temp;
use Net::Telnet;
use Net::SSL;
# use POSIX qw(setsid);
use Proc::Daemon;
use Time::HiRes qw (usleep);

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / Name Client ReceiveHandler STDINHandler Buffer BufferMessage
	DaemonP PIDFile PIDFileName DoNotDaemonize SSL Properties
	Debug RegisteredP Timeout /

  ];

sub init {
  my ($self, %args) = @_;
  $self->Timeout(1 * 24 * 3600);
  $self->RegisteredP(0);
  $self->Name($args{Name});
  $self->Debug($args{Debug});
  $self->STDINHandler($args{STDINHandler} || sub { $self->MySTDINHandler });
  $self->ReceiveHandler($args{ReceiveHandler});
  Event->io(
	    fd=>\*STDIN,
	    # timeout => $self->Timeout,
	    cb=> $self->STDINHandler,
	   );
  $Event::DIED = sub {
    # print ".\n";
  };
  $self->SSL(0);
}

sub Daemonize {
  my ($self, %args) = @_;
  if (! $self->DaemonP) {
    if ($self->Name ne "UniLang-Client" and ! $self->DoNotDaemonize) {
      print "Daemonizing ".$self->Name."\n";
      $self->DaemonP(1);
      Proc::Daemon::Init;

      # setup logging here too

      # add pid file
      my $pidfile = $0;
      $pidfile =~ s/.*\/([^\/]+)$/$1/;
      $self->PIDFileName($pidfile);

      $self->PIDFile
	(File::Pid->new
	 ({
	   file => "/var/run/".$self->PIDFileName.".pid",
	  }));
      if ( my $num = $pidfile->running ) {
      	die "Already running: $num\n";
      } else {
	$self->PIDFile->write;
      }

      #   # Pretty command line in ps
      #   # $0 = join (' ', @ARGV);

      #   # be a nice daemon and chdir to the root so we don't block any
      #   # unmount attempts
      #   chdir '/' or die "spamd: cannot chdir to /: $!\n";

      #   # Redirect in and out to the bit bucket
      #   open STDIN,  "</dev/null" or die "spamd: cannot read from /dev/null: $!\n";
      #   # open STDOUT, ">/dev/null" or die "spamd: cannot write to /dev/null: $!\n";
      #   open STDOUT, ">/tmp/unilang-irc-bot" or die "spamd: cannot write to /dev/null: $!\n";

      #   # Here we go...
      #   defined( my $pid = fork ) or die "spamd: cannot fork: $!\n";
      #   exit if $pid;
      #   setsid or die "spamd: cannot start new session: $!\n";

      #   # Now we can redirect the errors, too.
      #   open STDERR, '>&STDOUT' or die "spamd: cannot duplicate stdout: $!\n";
    }
  }
}

sub DESTROY {
  # print Dumper([@_]);
  # my ($self, %args) = @_;
  # $self->Deregister;
}

sub Register {
  my ($self, %args) = @_;
  my $properties = $args{Properties};
  delete $args{Properties};
  $self->Connect(%args);
  $self->Startup
    (Properties => $properties);
  $self->Daemonize;
  $self->RegisteredP(1);
}

sub Deregister {
  my ($self, %args) = @_;
  $self->Shutdown;
  $self->Disconnect;
  $self->RegisteredP(0);
}

sub Startup {
  my ($self, %args) = @_;
  # just create a message to send to unilang with your name
  $self->SendContents
    (
     Contents => "Register",
     Data => {
	      # _DoNotLog => 1,
	      Properties => $args{Properties},
	     },
    );
}

sub Shutdown {
  my ($self, %args) = @_;
  # just send a message to unilang with saying you are quiting
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
    # my $res = eval {

    $client = Net::Telnet->new
      (
       %args,
       Dump_Log => $f->filename,
      );
    # print Dumper({Client => $client});
    #     };
    #     if (! defined $res) {
    #       die "Me llamo ".$self->Name.".  Cannot open a connection.  Dying now.\n";
    #     }
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
    (
     fd => \*$handle,
     # timeout => $self->Timeout,
     cb => sub { $self->ClientHandler },
    );
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
    my $message = UniLang::Util::Message->new(Raw => $contents);
    if ($message) {
      if (exists $message->Data->{_Ping}) {
	if (exists $message->Data->{_Force}) {
	  print "Got a forced ping\n";
	  $self->Reply
	    (
	     Message => $message,
	     Contents => '',
	     Data => {
		      _Ping => 1,
		      _Force => 1,
		      _Success => 1,
		     },
	    );
	} else {
	  # this is a ping, ignore it
	}
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
  my $logmessage;
  if ($args{LogMessage}) {
    $logmessage = uc($args{LogMessage});
  } else {
    $logmessage = "SENDING";
  }
  if ($self->Debug) {
    print $logmessage." ".$message->Sender." to ".$message->Receiver.
      (exists $message->Data->{_TransactionID} ? " transactionid ".$message->Data->{_TransactionID}. " transactionsequence ".$message->Data->{_TransactionSequence} : "")."\n";
    # print join(",",sort keys %{$message->Data})."\n\n";
    print Dumper($message);
  }
  my $handle = $args{Handle};
  if (defined $handle) {
    printflush $handle $message->Generate;
  }
}

sub SendContents {
  my ($self,%args) = @_;
  my $message = UniLang::Util::Message->new
     (
      Sender => $args{Sender} || $self->Name,
      Receiver => $args{Receiver} || "UniLang",
      Date => $args{Date},
      Contents => $args{Contents},
      Data => $args{Data},
     );
  print Dumper($message) if $self->Debug;
  $self->Send
    (
     LogMessage => $args{LogMessage},
     Handle => $self->Client,
     Message => $message,
    );
}

sub Receive {
  my ($self,%args) = @_;
  my $handle = $args{Handle};
  my $line;
  my $contents;
  # $line = $handle->getline(Timeout => $self->Timeout);
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
  my $message = UniLang::Util::Message->new
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
     LogMessage => "Query",
     Handle => $self->Client,
     Message => $message,
    );
  my $conform = 0;
  my $responsemessage;
  do {
    $self->Listen(); # (TimeOut => 0.05);
    $responsemessage = $self->BufferMessage;
    $self->BufferMessage(undef);
    if (defined $responsemessage) {
      if (
	  exists $message->Data->{_TransactionID} and
	  exists $message->Data->{_TransactionSequence}) {
	if (
	    exists $responsemessage->Data->{_TransactionID} and
	    exists $responsemessage->Data->{_TransactionSequence}
	   ) {
	  if ($message->Data->{_TransactionID} eq $responsemessage->Data->{_TransactionID}) {
	    if ($message->Data->{_TransactionSequence} == ($responsemessage->Data->{_TransactionSequence} - 1)) {
	      $conform = 1;
	    }
	  }
	} else {
	  # it conforms because this message is obviously not sent
	  # using the transaction information, meaning it's from an
	  # older program, need to fix this
	  print "Warning, sender <<<".$responsemessage->Sender.">>> not using updated protocol\n";
	  $conform = 1;
	}
      }
    }
  } while (! $conform);
  $self->BufferMessage($bf);
  $self->ReceiveHandler($rh);
  if ($conform) {
    return $responsemessage;
  } else {
    return {
	    Success => 0,
	    Reason => {
		       "_TransactionID or _TransactionSequence not valid" => 1,
		      },
	   };
  }
}

sub QueryAgentReply {
  my ($self,%args) = @_;
  # print Dumper(\%args);
  if (defined $args{Message}->Data) {
    if (exists $args{Message}->Data->{_TransactionID}) {
      $args{Data}->{_TransactionID} = $args{Message}->Data->{_TransactionID};
      $args{Data}->{_TransactionSequence} = $args{Message}->Data->{_TransactionSequence} + 1;
    }
  }
  $args{Receiver} ||= $args{Message}->Sender;
  delete $args{Message};
  # print Dumper(\%args);
  $self->SendContents
    (
     LogMessage => "Reply",
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

sub NewEvent {
  my ($self,%args) = @_;
  my $type = $args{Type};
  Event->$type
    (
     %{$args{Args}},
    );
}

# add something here for manipulating Event, so like to add a timed
# callback or something

1;

