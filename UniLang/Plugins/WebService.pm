package UniLang::Plugins::WebService;

# I guess the model is, if you wish to connect to the remote unilang,
# you connect to your local unilang first...  how will that work with
# people who don't have unilang?  We'll need a client API for them

# have to prevent it from accessing itself and telling itself all the
# services it has

# need to make sure that if there are multiple agents with the same
# name on different hosts, it gets back to the correct one.

use base qw(UniLang::Plugin);

use UniLang::Util::Message;
use UniLang::Util::TempAgent;
use Sayer;

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / Name MySayer MyTempAgent SayerDBName /

  ];

sub init {
  my ($self,%args) = @_;
  print Dumper({WebServiceInit => \%args});
  $self->Name($args{Name});
  $self->SayerDBName
    ($args{SayerDBName} || "sayer_test");
  UniLang::Plugin->init(%args);
  $self->MyTempAgent
    (UniLang::Util::TempAgent->new
     (
      Name => $self->Name,
      Properties => {
		     Assertions => $self->GenerateAssertions,
		    },
      ReceiveHandler => sub {$self->UniLangReceive(@_)},
     ));
}

sub Start {
  my ($self,%args) = @_;
  # look into caching things with Sayer
  #   $self->MySayer
  #     (
  #      Sayer->new(DBName => $self->SayerDBName),
  #     );
  $self->MyTempAgent->MyAgent->Listen
    (
     TimeOut => $args{TimeOut},
    );
}

sub GenerateAssertions {
  my ($self,%args) = @_;
  return [
	  [
	   "hasAgentType",
	   $self->Name,
	   "UniLang::Plugins::WebService",
	  ],
	  [
	   "hasPackage",
	   $self->Name,
	   ref $self,
	  ],
	 ];
}

# sub WebServiceSend {
#   my ($self,%args) = @_;
#   # # $self->WebListen();
# }

sub WebServiceReceive {
  my ($self,%args) = @_;
  # if we have received a message from a remote unilang instance
  # through the webservice, call this function with the message, it
  # will query our unilang, and return the answer

  my $message = $args{Message};
  # if this is a response message, we will want to cache it
  # $self->MySayer->Add();
  $self->ProcessMessage
    (
     ImmediateSender => "WebService",
     Message => $message,
    )
}

sub UniLangSend {
  my ($self,%args) = @_;
  my $res = $self->MyTempAgent->MyAgent->SendContents
    (
     Receiver => $message->Receiver,
     Contents => $message->Contents,
    );

  # go into listen mode
  # # does it do it automatically?
  # # $self->MyTempAgent->Agent->Listen();
}

sub UniLangReceive {
  my ($self,%args) = @_;
  # if a client has connected, and we receive a message dedicated to
  # them, send it here

  # if this is a response message, we will want to cache it
  # $self->MySayer->Add();

  my $message = $args{Message};
  print Dumper({"UniLang::Plugins::WebService::UniLangReceive" => $message});
  $self->ProcessMessage
    (
     ImmediateSender => "UniLang",
     Message => $message,
    );
}

sub ProcessMessage {
  my ($self,%args) = @_;
  my $m = $args{Message};
  if ($m->Receiver eq $self->Name) { # message is addressed to us
    # act on the message
    my $it = $m->Contents;
    # what are some possible items here
    if ($it) {
      if ($it =~ /^echo\s*(.*)/) {
	$self->MyTempAgent->MyAgent->SendContents
	  (
	   Contents => $1,
	   Receiver => $m->{Sender},
	  );
      } elsif ($it =~ /^(quit|exit)$/i) {
	$self->MyTempAgent->MyAgent->Deregister;
	exit(0);
      } elsif ($it =~ /(list)/) {
        # list remote connections, so if you are a web server, list the
        # web clients connected to you

        # perhaps, translate the names...
      }
    }
  } else {

    # okay, maybe they are addressing it to a specific remote machine, check that it isn't of the form

    # # frdcsa.org::Enju2

    # which requires us to select the frdcsa.org connection, rewrite the
    # name to Enju2, and send it there.  also, the sender has to be
    # rewritten, I believe


    # Also, handle API recording, profiling, etc., ((gasp) restriction
    # and charging), permissions, etc.

    # now send it across the web service
    if ($args{ImmediateSender} eq "UniLang") {
      $self->WebServiceSend
	(
	 Message => $m,
	);
    } elsif ($args{ImmediateSender} eq "WebService") {
      $self->UniLangSend
	(
	 Message => $m,
	);
    }
  }
}

sub ListLocallyAvailableAgents {
  my ($self,%args) = @_;



  # if you are the server, you'll want to send a web service message
  # to the connecting client, asking it what it's unilang agents are,
  # and then return that information

  # we have to query the Local UniLang for this information
}

sub ListRemotelyAvailableAgents {
  my ($self,%args) = @_;
  # we should have a couple of pieces of information

  #   # all the remotely available and running Agents

  #   # all the remotely available but not running Agents

  #   # all the remote connections

  #   # all the performances and utilization of the remote connections (to
  #   # 	estimate which one will respond fastest)


  # if you are the server, you'll want to send a web service message
  # to the connecting client, asking it what it's unilang agents are,
  # and then return that information

  # we have to send a webservice message querying the remote UniLang
}

1;
