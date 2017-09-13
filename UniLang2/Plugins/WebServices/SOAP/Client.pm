package UniLang2::Plugins::WebServices::SOAP::Client;

use base qw(UniLang2::Plugins::WebServices::Client.pm);

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
