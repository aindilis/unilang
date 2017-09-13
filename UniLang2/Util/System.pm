package UniLang2::Util::System;

use Data::Dumper;

require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw (GetNewUnusedPort);

sub GetNewUnusedPort {
  my (%args) = @_;
  return 9000;
}

1;
