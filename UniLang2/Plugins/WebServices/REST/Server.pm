package UniLang2::Plugins::WebServices::REST::Server;

use base qw(UniLang2::Plugins::WebServices::Server);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw /  /

  ];

sub init {
  my ($self, %args) = @_;

}

1;
