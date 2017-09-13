package UniLang::Plugins::WebServices::REST::Client;

use base qw(UniLang::Plugins::WebServices::Client);

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
