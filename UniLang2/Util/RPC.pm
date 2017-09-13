package UniLang2::Util::RPC;

# cool stuff to do RPC over UniLang2, woo-hoo!

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Function Arguments /

  ];

sub init {
  my ($self, %args) = @_;
  $self->Function($args{Function});
  $self->Arguments($args{Arguments});
}

sub SPrint {
  my ($self, %args) = @_;
}

sub SParse {
  my ($self, %args) = @_;
}

sub Execute {
  my ($self, %args) = @_;
}

1;
