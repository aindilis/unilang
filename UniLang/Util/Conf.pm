package UniLang::Util::Conf;
use strict;
use Carp;
use Config::General;
use Getopt::Mixed;

use vars qw($VERSION);

$VERSION = '1.00';

sub new {
  my $class = shift;
  my $self = bless {}, $class;
  my $readable = sub { return $_[0] if -r $_[0] };
  $self->{rcfile} = shift ||
    &$readable("/etc/pse/pse.conf") ||
      &$readable("/home/jasayne/.pse.conf") || "";
  $self->init;
  $self;
}

sub init {
  my ( $self, %args ) = @_;
  my (%options, %config);

  # parse config file
  $self->{conf} = new Config::General($self->{rcfile});
  %config = $self->{conf}->getall;
  while ( my ( $key, $value ) = each %config ) {
    $self->{$key} = $value;
    print "<$key>\t<$value>\n";
  }

  # parse command line arguments
  Getopt::Mixed::init("logfile=s l>logfile");
  while ( my ( $option, $value ) = Getopt::Mixed::nextOption) {
    $self->{$option} = $value;
    print "<$option>\t<$value>\n";
  }
  Getopt::Mixed::cleanup;

  return $self;
}

1;
