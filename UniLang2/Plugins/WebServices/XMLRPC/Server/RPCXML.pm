package UniLang2::Plugins::WebServices::XMLRPC::Server::RPCXML;

use PerlLib::SwissArmyKnife;
use UniLang2::Util::Message;

use RPC::XML::Server;

use base qw(UniLang2::Plugins::WebServices::XMLRPC::Server);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / MyRPCXML WSHost WSPort /

  ];

sub init {
  my ($self,%args) = @_;
  $self->WSHost($args{WSHost} || "localhost");
  $self->WSPort($args{WSPort} || 10000);
  UniLang2::Plugins::WebServices::XMLRPC::Server->init(%args);
}

sub Start {
  my ($self,%args) = @_;
  $args{TimeOut} ||= 0.01;
  UniLang2::Plugins::WebServices::XMLRPC::Server->Start(%args);
  $self->StartServer
    (
     TimeOut => $args{TimeOut},
    );
}

sub StartServer {
  my ($self,%args) = @_;
  $self->MyRPCXML
    (RPC::XML::Server->new
     (
      no_default => 1,
      host => $self->WSHost,
      port => $self->WSPort,
     ));

  $self->MyRPCXML->add_method
    ({
      name => "Org.FRDCSA.UniLang2.Plugins.WebService.QueryAgent",
      version => "1.0",
      signature => [
		    'array',
		    'array array',
		   ],
      code => sub {
	my ($srv,$raw) = @_;
	my $message = UniLang2::Util::Message->new
	  (
	   Raw => $raw->[0],
	  );
	if ($message->Sender ne "Android-FRDCSA-Client" or $message->Receiver ne "Android-FRDCSA-Server") {
	  return 1;
	}
	my $message2 = UniLang2::Plugins::WebService->MyTempAgent->MyAgent->QueryAgent
	  (
	   Sender => "WS-Server-XMLRPC", # $message->Sender,
	   Receiver => $message->Receiver,
	   Date => $message->Date,
	   Contents => $message->Contents,
	  );
	$message2->Receiver("Android-FRDCSA-Client");
	return $message2->Generate;
      },

      help => "This is the WebService version of the QueryAgent
      method, that takes a message, sends it to it's destination, and
      waits for a reply, before sending that reply back.",

     });

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
      $conn = $self->MyRPCXML->{__daemon}->accept;
      UniLang2::Plugins::WebService->MyTempAgent->MyAgent->Listen
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
