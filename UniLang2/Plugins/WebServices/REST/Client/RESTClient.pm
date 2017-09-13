package UniLang2::Plugins::WebServices::REST::Client::RESTClient;

use base qw(UniLang2::Plugins::WebServices::REST::Client);

use REST::Client;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / /

  ];

sub init {
  my ($self,%args) = @_;

}

1;
