package UniLang2::Plugins::WebServices::XMLRPC::Client::XMLRPCLite;

use base qw(UniLang2::Plugins::WebServices::XMLRPC::Client);

use Data::Dumper;
use XMLRPC::Lite;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / WSHost WSPort /

  ];

sub init {
  my ($self,%args) = @_;
  $self->WSHost($args{WSHost} || "localhost");
  $self->WSPort($args{WSPort} || 10000);
  UniLang2::Plugins::WebServices::XMLRPC::Client->init(%args);
}

sub StartClient {
  my ($self,%args) = @_;
  print "HOWDY!\n";
}

sub WSSendContents {
  my ($self,%args) = @_;
  my $message = $args{Message};
  # we are to send this to the remote server
  my %funcargs = (
		  Message => $message,
		 );
  print Dumper
    (
     XMLRPC::Lite
     -> proxy("http://127.0.0.1:10000")
     -> call(
	     'Org.FRDCSA.UniLang2.Plugins.WebService.SendContents',
	     [%funcargs],
	    )
     -> result
    );
}

sub WSQueryAgent {
  my ($self,%args) = @_;
  my $message = $args{Message};
  # we are to send this to the remote server
  my %funcargs = (
		  Message => $message,
		 );
  my $response = XMLRPC::Lite
    -> proxy("http://127.0.0.1:10000")
      -> call(
	      'Org.FRDCSA.UniLang2.Plugins.WebService.QueryAgent',
	      [%funcargs],
	     )
	-> result;
  # if (exists $response->{Message} and
  #     ref $response->{Message} eq "UniLang2::Util::Message") {
  #    $self->WebServiceReceive
  #     (Message => $response);
  # }
  return $response;
}

1;
