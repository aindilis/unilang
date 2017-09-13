package UniLang2::Plugins::WebServices::XMLRPC::Client;

use base qw(UniLang2::Plugins::WebServices::Client);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / /

  ];

sub init {
  my ($self,%args) = @_;
  UniLang2::Plugins::WebServices::Client->init(%args);
}

1;
