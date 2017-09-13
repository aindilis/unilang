package UniLang::Util::Comm;

use strict;
use Carp;

sub Send {
  my $message = shift;
  print $handle $message->Generate;
}

sub Receive {
  my $line;
  my $contents;
  $line = $handle->getline();
  $contents .= $line;
  my $closure = $line;
  $closure =~ s/^</<\//;
  do {
    $line = $handle->getline();
    $contents .= $line;
  } while ($line ne $closure);
  return $contents;
}

1;
