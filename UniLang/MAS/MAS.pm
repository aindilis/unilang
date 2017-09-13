package UniLang::MAS::MAS;

use UniLang::MAS::Server;
use UniLang::MAS::Agent;
use UniLang::Util::Log;
use UniLang::Util::Message;

use Data::Dumper;
use String::ShellQuote;
use XML::Twig;

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Host Port Server Log Agents AgentMessageQueue /

  ];

{
  my $server;
  my $log = UniLang::Util::Log->new;

  sub init {
    my ($self,%args) = @_;
    $self->Host($args{Host} || "localhost");
    $self->Port($args{Port} || "9000");
    $server = UniLang::MAS::Server->new
      (
       MAS => $self,
       Host => $self->Host,
       Port => $self->Port,
       Conf => $args{Conf},
      );
    $self->Server($server);
    $self->Log($log);
    $self->Agents({});
    $self->AgentMessageQueue({});
  }

  sub UNIVERSAL::get_server { $server }
  sub UNIVERSAL::get_logg { $log }
}

sub Start {
  my ($self,%args) = @_;
  $self->Server->Listen;
}

sub AddAgent {
  my ($self,%args) = @_;
  my $agents = $self->Agents;
  my $agent = $args{Agent};
  $agents->{$agent->Name} = $agent;
  $self->Agents($agents);
}

sub RemoveAgent {
  my ($self,%args) = @_;
  my $agents = $self->Agents;
  $agents->{$args{Agent}->Name} = undef;
  $self->Agents($agents);
}

sub PrintAllAgents {
  my ($self,%args) = @_;
  my $twig = XML::Twig->new(pretty_print => 'indented');
  my $agents = XML::Twig::Elt->new('agents');
  foreach my $agent ($self->ListAgents) {
    $agent->GrowTwig->paste('last_child',$agents);
  }
  return $agents->sprint;
}

sub ListAgents {
  my ($self,%args) = @_;
  return values %{$self->Agents};
}

sub QueryAgents {
  my ($self,%args) = @_;
}

sub IsaAgent {
  my ($self, %args) = @_;
  # add a test here for whether the agent is still valid
  return $self->Agents->{$args{Name}};

  #   # BROKEN:
  #   if (exists $self->Agents->{$args{Name}}) {
  #     if ($self->Agents->{$args{Name}}->Client->Handle->connected) {
  #       print "Still on\n";
  #       return $self->Agents->{$args{Name}};
  #     } else {
  #       $self->DeRegister(Agent => $args{Name});
  #     }
  #   }
}

  # sub Reply {
  #   my ($self, $contents, %args) = @_;
  #   my $client = $args{Client};
  #   my $message = $args{Message};
  #   my $command = $args{Command};
  #   my $newmessage = UniLang::Util::Message->new
  #     (
  #      Sender => "UniLang",
  #      Receiver => $message->Sender,
  #      Contents => $contents,
  #      Data => $args{Data},
  #     );
  #   $self->Server->Send(Message => $newmessage,
  # 		      Handle => $client->Handle);
  # }

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

sub SendContents {
  my ($self,%args) = @_;
  unless ($args{Receiver}) {
    print "ERROR no receiver\n";
    return;
  }
  my $message = UniLang::Util::Message->new
     (
      Sender => $args{Sender} || "UniLang",
      Receiver => $args{Receiver},
      Date => $args{Date},
      Contents => $args{Contents},
      Data => $args{Data},
     );
  # print Dumper($message);
  $self->Server->Send
    (
     Message => $message,
     Handle => $args{Client}->Handle,
    );
}

sub ProcessCommunicationEvent {
  my ($self, %args) = @_;
  my $message = $args{Message};
  my $client = $args{Client};
  my $sender = $message->Sender;

  # log the message
  my $entryid = $self->Log->Commit($message);

  # # if an agent is requesting it, send back the entry id
  #   if (exists $message->Data->{_GetEntryID}) {
  #     $args{Data} = {
  # 		   EntryID => $entryid,
  # 		  };
  #     $self->Reply("",%args);
  #   }

  if (exists $message->Data->{_GetEntryID}) {
    $self->QueryAgentReply
      (
       Client => $client,
       Contents => "",
       Data => {
		EntryID => $entryid,
		_DoNotLog => 1,
	       },
       Message => $message,
      );
  }

  # don't proceed with processing if instructed not to
  return if exists $message->Data->{_DoNotAct};

  # verify name
  if ($client->Agent->Name eq $client->Name) {
    # verify that sender is unique
    my $agent = $self->IsaAgent(Name => $sender);
    if ($agent) {
      # check that it is alive
      # send a ping to this agent
      my $message = UniLang::Util::Message->new
	(
	 Sender => "UniLang",
	 Receiver => $sender,
	 Contents => "",
	 Data => {_Ping => 1},
	);
      $self->Server->Send
	(Message => $message,
	 Handle => $agent->Client->Handle);
      if ($agent->Client->Handle->connected) {
	print "Name already taken: <$sender>\n";
      } else {
	print "Agent <$sender> was already disconnected\n";
	$self->DeRegister(Agent => $sender);
	$self->Register
	  (
	   Name => $sender,
	   Client => $client,
	  );
      }
    } else {
      # $client->Agent->Name($sender);
      # $self->AddAgent(Agent => $client->Agent);
      $self->Register
	(
	 Name => $sender,
	 Client => $client,
	);
    }
  }
  if ($client->Agent->Name ne $sender) {
    # bogus message? how has sender name changed?
    ########################################
    print "bogus message, not processing\n";
  } else {
    # look this up to see if such an agent exists
    my $receiver = $message->Receiver;
    # ID, Sender, Receiver, Date, Contents
    # for now simply route it to the agent by that name
    if ($receiver eq "UniLang") {
      # if addressed to UniLang, call a special function
      $self->AnalyzeContents(Client => $client,
			     Message => $message);
    } else {
      # lookup Agent and route message to it
      # add autovivification here
      $self->SendMessageWithAutoVivification
	(
	 Message => $message,
	 Name => $receiver,
	 Client => $args{Client},
	);
      #       if (my $agent = $self->IsaAgent(Name => $receiver)) {
      # 	# send the message to that agent
      # 	# for now simply indicate that we would send this to that agent
      # 	# print "send to ".$agent->Name."\n";
      # 	# print $message->Generate;
      # 	$self->Server->Send(Message => $message,
      # 			    Handle => $agent->Client->Handle);
      #       }
    }
  }
}

sub AnalyzeContents {
  my ($self, %args) = @_;
  my $client = $args{Client};
  my $message = $args{Message};
  # print $message->Generate;;
  my $contents = $message->Contents;
  # check contents to see whether it is a command
  if ($contents =~ /^UniLang,\s*(.*)$/i) {
    # it is a command, check permissions and execute it
    $self->ProcessCommand(Client => $client,
			  Message => $message,
			  Command => $1);
  } elsif ($contents =~ /^(\S+),\s*(.*)$/s) {
    my ($name,$contents) = ($1,$2);
    # check contents to see whether it is a direct address
    # may be a message to an agent, look up agents
    my $message2 = UniLang::Util::Message->new
      (Message => $message);
    $message2->Sender($message->Sender);
    $message2->Receiver($name);
    $message2->Contents($contents);
    $self->SendMessageWithAutoVivification
      (
       Name => $name,
       Message => $message2,
       Client => $args{Client},
      );
  } elsif ($contents =~ /^Register$/) {
    # maybe write this to a log and/or update our registered agents?
    print "Agent ".$message->Sender." registered\n";
    # check here if there are messages for them
    if (exists $self->AgentMessageQueue->{$message->Sender}) {
      my $list = $self->AgentMessageQueue->{$message->Sender};
      while (@$list) {
	$self->Server->Send
	  (Message => (shift @$list),
	   Handle => $client->Handle);
      }
    }
  } elsif ($contents =~ /^Deregister$/) {
    # maybe write this to a log and/or update our registered agents?
    $self->DeRegister(Agent => $message->Sender);
  } else {
    $self->ProcessArbitraryMessage
      (Process => $contents,
       Message => $message);
  }
}

sub SendMessageWithAutoVivification {
  my ($self, %args) = @_;
  my $name = $args{Name};
  my $message = $args{Message};
  if (my $agent = $self->IsaAgent(Name => $name)) {
    # send the message to that agent
    # for now simply indicate that we would send this to that agent
    $self->Server->Send(Message => $message,
			Handle => $agent->Client->Handle);
  } else {
    # add here code to lookup whether such an agent can be started
    if ($name =~ /^[\w-]+$/ and length($name) < 1024) {
      # if the agent is already being started, we need simply append
      # the message to AgentMessageQueue
      if (exists $self->AgentMessageQueue->{$name}) {
	push @{$self->AgentMessageQueue->{$name}}, $message;
      } else {
	my $res = `/var/lib/myfrdcsa/codebases/internal/unilang/start --check $name`;
	if ($res =~ /agent exists/) {
	  # start the agent
	  my $command = "/var/lib/myfrdcsa/codebases/internal/unilang/start -u ".$self->Host." ".$self->Port." -a $name";
	  print Dumper({AndyMessage => $message});
	  if ($message->{Data}{_ExtraOptions}) {
	    print Dumper({ExtraOptions => $message->{Data}{_ExtraOptions}});
	    $command .= ' -e '.shell_quote($message->{Data}{_ExtraOptions});
	  }
	  print $command."\n";
	  system $command;
	  # if the agent is there, hold the message until it is ready
	  # add the message to the agent queue, when it registers, send the message on
	  if (! exists $self->AgentMessageQueue->{$name}) {
	    $self->AgentMessageQueue->{$name} = [$message];
	  } else {
	    push @{$self->AgentMessageQueue->{$name}}, $message;
	  }
	} else {
	  $self->Reply("No one here by that name.\n",%args);
	}
      }
    } else {
      $self->Reply("Invalid agent name.\n",%args);
    }
  }
}

sub Register {
  my ($self, %args) = @_;
  $args{Client}->Agent->Name($args{Name});
  $self->AddAgent(Agent => $args{Client}->Agent);
}

sub DeRegister {
  my ($self, %args) = @_;
  if ($self->IsaAgent(Name => $args{Agent})) {

    # WANT to remove everything, refactor when working
    # shutdown the handle for the agent
    my $agent = $self->Agents->{$args{Agent}};
    my $handle = $self->Server->Clients->{$agent->Client->Name}->Handle;
    $self->Server->ReadSet->remove($handle);
    $handle->shutdown(1) if $handle->connected;
    # remove from clients information
    delete $self->Server->Clients->{$agent->Client->Name};
    delete $self->Agents->{$args{Agent}};
    delete $self->AgentMessageQueue->{$args{Agent}};

    print "Agent $args{Agent} deregistered\n";
  } else {
    # message that that is not an agent
    print "Cannot Deregister non-existant agent $args{Agent}\n";
  }
}

sub Reply {
  my ($self, $contents, %args) = @_;
  my $client = $args{Client};
  my $message = $args{Message};
  my $command = $args{Command};
  my $newmessage = UniLang::Util::Message->new
    (
     Sender => "UniLang",
     Receiver => $message->Sender,
     Contents => $contents,
     Data => $args{Data},
    );
  $self->Server->Send(Message => $newmessage,
		      Handle => $client->Handle);
}

sub ProcessCommand {
  my ($self, %args) = @_;
  my $client = $args{Client};
  my $message = $args{Message};
  my $command = $args{Command};
  if ($command =~ /^print$/) {
    $self->Reply($self->PrintAllAgents,%args);
  } elsif ($command =~ /^deregister (.+)$/) {
    $self->DeRegister(Agent => $1);
    #   } elsif ($command =~ /^spoof-unilang-client (.+)$/) {
    #     my $realsender = $message->{Sender};
    #     $message->{Sender} = "UniLang-Client";
    #     # $self->ProcessArbitraryMessage
    #     #  (Message => $message);
    #     # send back the id of this message
    #     $self->SendMessageWithAutoVivification
    #       (
    #        Message => UniLang::Util::Message->new
    #        (
    # 	Sender => "UniLang",
    # 	Receiver => $realsender,
    # 	Contents => $message->{ID},
    # 	Data => {_DoNotLog => 1},
    #        ),
    #        Name => $realsender,
    #       );
  } elsif ($command =~ /^echo (.+)$/) {
    print Dumper($1);
  } elsif ($command =~ /^stats$/) {
    print Dumper
      ({
	ReadsetSize => scalar $self->Server->ReadSet->handles,
	ReadSet => $self->Server->ReadSet,
	Delay => $self->Server->Delay,
       });
  } elsif ($command =~ /^quit|exit$/) {
    exit(0);
  }
}

sub ProcessArbitraryMessage {
  my ($self, %args) = @_;
  # this will be interesting, here we will add handlers
  # this is where we do classification
  # for now just write it to unilang logfile as normal
  # print "<<$args{Message}->{Date}> <$args{Message}->{Contents}>>\n";
  if ($args{Message}->{Sender} eq "Corpus") {
    # only use this if sending back hash (not gloss)
    # my $hash = eval $args{Message}->{Contents};
    print $args{Message}->{Contents}."\n";
  }
  if ($args{Message}->{Sender} eq "UniLang-Client") {
    # go ahead and classify this message
    # if it has an ID, send that
    if (defined $args{Message}->{ID}) {
      my $newmessage = UniLang::Util::Message->new
	(
	 Sender => "UniLang",
	 Receiver => "Corpus",
	 Contents => "route ".$args{Message}->{ID},
	 Data => {
		  _DoNotLog => 1,
		 },
	);
      $self->SendMessageWithAutoVivification
	(Message => $newmessage,
	 Name => "Corpus");
      #      if (exists $self->Agents->{Corpus}) {
      # 	$self->Server->Send
      # 	  (Message => $newmessage,
      # 	   Handle => $self->Agents->{Corpus}->Client->Handle);
      #      }
    }
  }
}

1;
