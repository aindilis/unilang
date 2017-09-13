package UniLang::MAS::Classification;

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker new_with_init => 'new',
  get_set => [ qw / LogFile / ];

sub init {
  my ($self,%args) = (shift,@_);
  
}

sub LogMessage {
  my ($self,%args) = (shift,@_);
  # log the classification results for a given message

}

sub {

}

1;
