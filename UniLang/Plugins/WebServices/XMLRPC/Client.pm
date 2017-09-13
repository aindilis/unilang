package UniLang::Plugins::WebServices::XMLRPC::Client;

use base qw(UniLang::Plugins::WebServices::Client);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / /

  ];

sub init {
  my ($self,%args) = @_;
  UniLang::Plugins::WebServices::Client->init(%args);
}

1;
