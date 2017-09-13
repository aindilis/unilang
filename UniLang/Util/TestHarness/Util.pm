package UniLang::Util::TestHarness::Util;

use PerlLib::SwissArmyKnife;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / LogFile MyFilehandle /

  ];

sub init {
  my ($self,%args) = @_;
  $self->LogFile("/tmp/unilang-test/log.txt");
  my $dirname = dirname($self->LogFile);
  if (! -d $dirname) {
    DoCommand("mkdir -p ".shell_quote($dirname));
  }
  $self->MyFilehandle(IO::File->new);
  $self->MyFilehandle->open(">>".$self->LogFile) or die "Cannot open logfile ".$self->LogFile."\n";
}

sub Log {
  my ($self,%args) = @_;
  my $fh = $self->MyFilehandle;
  print $fh $args{Message}."\n";
}

1;
