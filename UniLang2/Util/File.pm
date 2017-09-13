package UniLang2::Util::File;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use strict;
use File::Basename;
use Class::MethodMaker new_with_init => 'new',
  get_set       => [ qw / Name Spec / ];

sub init {
  my ($self,$file) = (shift,shift);
  $self->Name($file);
}

sub DirName {
  my ($self,%args) = (shift,@_);
  dirname($self->Name);
}

sub Open {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Writable {
  my ($self,%args) = (shift,@_);
  -W $self->Name;
}

sub Exists {
  my ($self,%args) = (shift,@_);
  -e $self->Name;
}

sub Directory {
  my ($self,%args) = (shift,@_);
  -d $self->Name;
}

1;
