package UniLang::Util::Messages;

use UniLang::Util::Message;

use Data::Dumper;
use utf8;
use Error qw(:try);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Messages / ];

sub init {
  my ($self, %args) = @_;
  my @messages;
  foreach my $rawPart ($self->GetMessages(Raw => $args{Raw})) {
    push @messages, UniLang::Util::Message->new(Raw => $rawPart);
  }
  $self->Messages(\@messages);
}

sub GetMessages {
  my ($self, %args) = @_;
  my $raw = $args{Raw};
  my @rawParts;
  while ($raw) {
    try {
      my $message = UniLang::Util::Message->new(Raw => $raw);
      push @rawParts, $raw;
      $raw = "";
    }
      catch Error with {
	my $errorMessage = (shift);
	if ($errorMessage->{'-text'} =~ /^\s*junk after document element at line (\d+), column (\d+), byte (\d+) at /s) {
	  my $line = $1;
	  my $column = $2;
	  my $byte = $3;
	  my $regex = "^(.{$byte})(.*)\$";
	  if ($raw =~ /$regex/sm) {
	    push @rawParts, $1;
	    $raw = $2;
	  }
	}
      };
  }
  return @rawParts;
}

1;

