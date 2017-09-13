package UniLang2::Plugins::WebServices::SOAP::Server::SOAPWSDL;

use base qw(UniLang2::Plugins::WebServices::SOAP::Server);

use base qw(HTTP::Server::Simple::CGI);
use MyServer::TestService::TestPort;

sub handle_request {
  my ($self, $cgi) = @_;
  my $server = MyServer::TestService::TestPort->new
    ({
      dispatch_to => 'main',
      transport_class => 'SOAP::WSDL::Server::Simple',
     });
  $server->handle($cgi);
}

my $httpd = __PACKAGE__->new();
$httpd->run();

1;
