package UniLang::Util::LogFile;

use strict;
use vars qw($VERSION @ISA);

$VERSION = '1.00';
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

sub ReadHeader {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Version {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Validate {
  my $self = shift;
  # check that it is of the right format
  print "<<<".$self->Name.">>>\n";
  if (0) {
    $self->Open;
    $self->ReadHeader;
    if ($self->Version eq $self->Version) {
      return 1;
    }
  }
  return 1;
}

1;
