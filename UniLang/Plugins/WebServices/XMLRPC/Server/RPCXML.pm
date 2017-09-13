package UniLang::Plugins::WebServices::XMLRPC::Server::RPCXML;

use PerlLib::SwissArmyKnife;
use UniLang::Util::Message;

use Time::HiRes qw( time );
use RPC::XML::Server;

use base qw(UniLang::Plugins::WebServices::XMLRPC::Server);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / MyRPCXML WSHost WSPort StartTime /

  ];

sub init {
  my ($self,%args) = @_;
  $self->WSHost($args{WSHost} || "localhost");
  $self->WSPort($args{WSPort} || 10000);
  UniLang::Plugins::WebServices::XMLRPC::Server->init(%args);
}

sub Start {
  my ($self,%args) = @_;
  $args{TimeOut} ||= 0.01;
  UniLang::Plugins::WebServices::XMLRPC::Server->Start(%args);
  $self->StartServer
    (
     TimeOut => $args{TimeOut},
    );
}

sub StartServer {
  my ($self,%args) = @_;
  $self->StartTime(time());

  $self->MyRPCXML
    (RPC::XML::Server->new
     (
      no_default => 1,
      host => $self->WSHost,
      port => $self->WSPort,
     ));

  $self->MyRPCXML->add_method
    ({
      name => "Org.FRDCSA.UniLang.Plugins.WebService.QueryAgent",
      version => "1.0",
      signature => [
      		    'array string',
      		    'array array',
      		   ],
      code => sub {
	my ($srv,$raw) = @_;
	my $contents = $raw->[0];
	$contents =~ s/^<org.frdcsa.unilang.util.Message>/<message>/s;
	$contents =~ s/<\/org.frdcsa.unilang.util.Message>\n$/<\/message>/s;
	print Dumper({Contents => $contents});
	my $message = UniLang::Util::Message->new
	  (
	   Raw => $contents,
	  );
	if (($message->Sender ne "Android-FRDCSA-Client" or $message->Receiver ne "Android-FRDCSA-Server") and
	    ($message->Sender ne "Alexa-Skill-FLP" or $message->Receiver ne "Agent1")) {
	  return 1;
	}
	my $message2 = UniLang::Plugins::WebService->MyTempAgent->MyAgent->QueryAgent
	  (
	   Sender => "WS-Server-XMLRPC", # $message->Sender,
	   Receiver => $message->Receiver,
	   Date => $message->Date,
	   Contents => $message->Contents,
	   Data => $message->Data,
	  );
	if ($message->Sender eq "Android-FRDCSA-Client" or $message->Receiver eq "Android-FRDCSA-Server") {
	  $message2->Receiver("Android-FRDCSA-Client");
	}
	if ($message->Sender eq "Alexa-Skill-FLP" or $message->Receiver eq "Agent1") {
	  $message2->Receiver("Alexa-Skill-FLP");
	}
	print Dumper({Message => $message2->Generate});
	return $message2->Generate;
      },

      help => "This is the WebService version of the QueryAgent
      method, that takes a message, sends it to it's destination, and
      waits for a reply, before sending that reply back.",

     });

  print Dumper({Methods => $self->MyRPCXML->list_methods});

  $self->MyRPCXML->add_default_methods;
  # print Dumper($self->MyRPCXML);

  $self->RPCXMLServerLoop
    (
     TimeOut => $args{TimeOut},
    );
}

sub RPCXMLServerLoop {
  my ($self,%args) = @_;
  if ($self->MyRPCXML->{__daemon}) {
    my ($conn, $req, $resp, $reqxml, $return, $respxml, $exit_now,
	$timeout);

    # Localize and set the signal handler as an exit route
    my @exit_signals;

    if (exists $args{signal} and $args{signal} ne 'NONE') {
      @exit_signals =
	(ref $args{signal}) ? @{$args{signal}} : $args{signal};
    } else {
      push @exit_signals, 'INT';
    }

    local @SIG{@exit_signals} = (sub { $exit_now++ }) x @exit_signals;

    $self->MyRPCXML->started('set');
    $exit_now = 0;
    $timeout  = $self->MyRPCXML->{__daemon}->timeout(0.1);
    while (!$exit_now) {
      if (exists $UNIVERSAL::conf->{'-W'}) {
	my $delay = $UNIVERSAL::conf->{'-W'} || 1000;
	$delay = $delay / 1000.0;
	if (time() > ($self->StartTime + $delay)) {
	  UniLang::Plugins::WebService->MyTempAgent->MyAgent->Deregister;
	  exit(0);
	}

      }

      $conn = $self->MyRPCXML->{__daemon}->accept;
      UniLang::Plugins::WebService->MyTempAgent->MyAgent->Listen
	  (
	   TimeOut => $args{TimeOut},
	  );
      last if $exit_now;
      next unless $conn;

      $conn->timeout($self->MyRPCXML->timeout);
      $self->MyRPCXML->process_request($conn);
      $conn->close;
      undef $conn;		# Free up any lingering resources
    }

    $self->MyRPCXML->{__daemon}->timeout($timeout) if defined $timeout;
  }
}

1;
