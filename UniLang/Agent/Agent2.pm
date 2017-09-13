package UniLang::Agent::Agent;

use PerlLib::SwissArmyKnife;
use UniLang2::Util::Message;

use AnyEvent;
use Event qw (loop unloop); # one_event
use File::Pid;
use File::Temp;


# use IO::Handle;
# use IO::Socket::SSL;

use Linux::Inotify2;
use Net::Telnet;

# use Net::SSL;
# use POSIX qw(setsid);

use Proc::Daemon;
use Time::HiRes qw (usleep alarm tv_interval);


use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Name Client ReceiveHandler STDINHandler Buffer BufferMessage
	DaemonP PIDFile PIDFileName DoNotDaemonize SSL Properties
	Debug RegisteredP

	Host Port

	MyINotify MessageDir IncomingDir ServerIncomingDir
	MessageCounter Timeout /

  ];

sub init {
  my ($self, %args) = @_;
  $self->Timeout(1 * 24 * 3600);
  $self->RegisteredP(0);
  $self->Name($args{Name});
  $self->Debug($args{Debug});
  $self->STDINHandler($args{STDINHandler} || sub { $self->MySTDINHandler });
  $self->ReceiveHandler($args{ReceiveHandler});
  #   Event->io(fd=>\*STDIN,
  # 	    timeout => $self->Timeout,
  # 	    cb=> $self->STDINHandler);
  #   $Event::DIED = sub {
  #     # print ".\n";
  #   };
  $self->SSL(0);
}

sub Daemonize {
  my ($self, %args) = @_;
  return;
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
      #   open STDOUT, ">/tmp/unilang2-irc-bot" or die "spamd: cannot write to /dev/null: $!\n";

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
  # just create a message to send to unilang2 with your name
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
  # just send a message to unilang2 with saying you are quiting
  if (defined $self->Client) {
    $self->SendContents
      (Contents => "Deregister");
  }
}

sub Connect {
  my ($self, %args) = @_;
  $self->Host
    ($args{Host} || "localhost");
  $self->Port
    ($args{Port} || "9000");
  $self->MessageDir("/var/lib/myfrdcsa/codebases/internal/unilang/data/messages");
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
  $self->IncomingDir(ConcatDir($self->MessageDir,"clients",$self->Name,"incoming"));
  MkDirIfNotExists
    (
     Directory => $self->IncomingDir,
    );
  $self->ServerIncomingDir(ConcatDir($self->MessageDir,"servers",$self->Port,"incoming"));
  $self->MessageCounter(0);
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
	 my $file = basename($name);
	 if ($file =~ /^(.+)-(\d+)$/) {
	   my $clientname = $1;
	   $self->ClientHandler(Contents => $raw);
	   # system "rm ".shell_quote($name);
	 }
       }
     },
    );
}

sub Disconnect {
  my ($self, %args) = @_;
  # we can remove the directory
  # safely remove the directory

  #   if (defined $self->Client) {
  #     $self->Client->close;
  #   }
}

sub OneEvent {
  # V2
  my ($self,%args) = @_;
  # one_event($args{TimeOut});
}

sub Listen {
  my ($self,%args) = @_;
  if (exists $args{TimeOut}) {
#     $self->MyINotify->blocking(0);
#     do {
#       $self->MyINotify->poll;
#       usleep(5000);
#     } while ($i++ < 100);
#     $self->MyINotify->blocking(1);
#   } else {
    $self->MyINotify->poll;
  }
  # $self->PIDFile->remove if $self->PIDFile;
}

# sub Listen {
#   my ($self,%args) = @_;
#   if (exists $args{TimeOut}) {
#     $self->MyINotify->blocking(0);
#     my $t0 = [gettimeofday];
#     my $elapsed;
#     do {
#       $self->MyINotify->poll;
#       usleep(5000);
#       $elapsed = tv_interval($t0,[gettimeofday]);
#       print Dumper($elapsed);
#     } while ($elapsed < $args{TimeOut});
#     $self->MyINotify->blocking(1);
#   } else {
#     $self->MyINotify->poll;
#   }
#   # $self->PIDFile->remove if $self->PIDFile;
# }

sub ListenAlarm {
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

sub UnListen {
  my ($self,%args) = @_;
  # # V2
  # $e->w->cancel;
}

sub GetDate {
  my ($self,%args) = @_;
  # V2
  chomp (my $date = `date`);
  return $date;
}

sub MySTDINHandler {
  my ($self,%args) = @_;
  # V2
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
  # V2
  my $contents = $args{Contents};
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
  # V2
  print $self->Name . "\n";
}

sub Send {
  my ($self,%args) = @_;
  # V2
  my $fh = IO::File->new();
  my $filename = ConcatDir($self->ServerIncomingDir,$self->Name."-".$self->MessageCounter);
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
  $self->MessageCounter($self->MessageCounter + 1);
}

sub SendContents {
  my ($self,%args) = @_;
  # V2
  my $message = UniLang2::Util::Message->new
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
     Message => $message,
    );
}

# sub Receive {
#   my ($self,%args) = @_;
#   my $handle = $args{Handle};
#   my $line;
#   my $contents;
#   # $line = $handle->getline(Timeout => $self->Timeout);
#   $line = $handle->getline();
#   if ($line) {
#     $contents .= $line;
#     my $closure = $line;
#     $closure =~ s/^\</\<\//;
#     do {
#       $line = $handle->getline();
#       $contents .= $line;
#     } while ($line ne $closure);
#     return $contents;
#   }
#   return;
# }

sub Reply {
  my ($self,%args) = @_;
  # V2
  $self->SendContents
    (
     Receiver => $args{Message}->Sender,
     Contents => $args{Contents},
     Data => $args{Data},
    );
}

sub GetTransactionID {
  # V2
  return rand();
}

sub QueryAgent {
  my ($self,%args) = @_;
  # V2
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
     LogMessage => "Query",
     Message => $message,
    );
  my $conform = 0;
  my $responsemessage;
  do {
    $self->Listen();
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
  # V2
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
  # V2
  $self->Deregister;
  $self->Register
    (
     Host => "localhost",
     Port => "9000",
    );
}

sub ReceiveToBuffer {
  my ($self,%args) = @_;
  # V2
  $self->Buffer($args{Message}->Contents);
  $self->UnListen;
}

sub ReceiveToBufferMessage {
  my ($self,%args) = @_;
  # V2
  $self->BufferMessage($args{Message});
  $self->UnListen;
}

sub ChangeName {
  my ($self,%args) = @_;
  # V2
  $self->Shutdown;
  $self->Name($args{Name});
  $self->Startup;
}

sub Ping {
  my ($self,%args) = @_;
  # V2
  return 1;
}

1;

# sub ListenOld {
#   my ($self,%args) = @_;
#   if (exists $args{TimeOut}) {
#     $self->MyINotify->poll;
#    } else {
#     $self->MyINotify->poll;
#   }
#   # $self->PIDFile->remove if $self->PIDFile;
# }

# sub ListenNew {
#   my ($self,%args) = @_;
#   if (exists $args{TimeOut}) {
#     $self->MyINotify->blocking(0);
#     my $time = gettimeofday();
#     $self->MyINotify->poll;

#     eval {
#       local $SIG{ALRM} = sub { die "alarm\n" }; # NB: \n required
#       alarm $args{TimeOut};

#       alarm 0;
#     };
#     if ($@) {
#       print "Error with Listen\n" unless $@ eq "alarm\n";   # propagate unexpected errors
#       # timed out
#     }
#     else {
#       # didn't
#     }
#   } else {
#     $self->MyINotify->poll;
#   }
#   # $self->PIDFile->remove if $self->PIDFile;
# }
