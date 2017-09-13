package UniLang2::Plugin;

# base class for unilang2 plugins.  wait a minute, do we really need to
# have a separate agent here, or can we simply access the MAS
# internals?  The agent seems to be an easier to accomplish, and
# cleaner separation..., but may make things harder

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / Debug /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Debug(1);
}

1;
