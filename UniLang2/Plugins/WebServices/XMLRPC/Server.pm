package UniLang2::Plugins::WebServices::XMLRPC::Server;

use base qw(UniLang2::Plugins::WebServices::Server);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / /

  ];

sub init {
  my ($self,%args) = @_;
  UniLang2::Plugins::WebServices::Server->init(%args);
}

sub Start {
  my ($self,%args) = @_;
  UniLang2::Plugins::WebServices::Server->Start(%args);
}

1;
