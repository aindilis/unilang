#!/usr/bin/perl -w

use Do::Misc::ListProcessor2;
use Manager::Misc::Light;
# use Manager::Misc::Light2;
use PerlLib::SwissArmyKnife;

# This needs to be vastly improved, we need to extract out strings
# instead of the simple domain, then we need to convert as necessary
# to hashes

# In fact, have a generalized convert system, that can convert between
# XML, java, perl, emacs, etc data structures

use Data::Dumper;

my $listprocessor = Do::Misc::ListProcessor2->new;
my $light = Manager::Misc::Light->new;
# my $light2 = Manager::Misc::Light2->new;

sub Convert {
  my (%args) = @_;
  my $contents = $args{Contents};
  my $domain = $light->Parse(Contents => $contents);
  my $res = $listprocessor->ProcessDomainNew(Domain => $domain);
  return $res->{ReturnDomain};
}

if (1) {
  my $contents = read_file($ARGV[0]);
  my $res = Convert(Contents => $contents);
  my $res1 = AddDepthInformation
    (Item => $res->[0]);
  # print Dumper($res1);
  # exit(0);
  print Dumper
    (ConvertNestedListsToHash
     (Item => $res1));
}

sub AddDepthInformation {
  my (%args) = @_;
  my $item = $args{Item};
  my $ref = ref $item;
  my @return;
  if ($ref eq "ARRAY") {
    my $maxdepth = 0;
    foreach my $subitem (@$item) {
      my $res = AddDepthInformation
	(
	 Item => $subitem,
	);
      my $depth = $res->[0] || 0;
      push @return, $res;
      if (($depth + 1) > $maxdepth) {
	$maxdepth = $depth + 1;
      }
      # print "<$maxdepth>\n";
    }
    return [
	    $maxdepth,
	    @return,
	   ];
  } else {
    return $item;
  }
}

sub ConvertNestedListsToHash {
  my (%args) = @_;
  my $return = {};
  my $item = $args{Item};
  my $ref = ref $item;
  if ($ref eq "ARRAY") {
    my $depth = shift @$item;
    my $key = shift @$item;
    if ($depth == 1) {
      $return->{$key} = 1;
    } else {
      foreach my $subitem (@$item) {
	my $res = ConvertNestedListsToHash
	  (
	   Item => $subitem,
	  );
	foreach my $key2 (keys %$res) {
	  $return->{$key}->{$key2} = $res->{$key2};
	}
      }
    }
  }
  return $return;
}

if (0) {
  my $sample =
    [
     'accounts',
     [
      'Gmail accounts',
      [
       'adougher9'
      ],
      [
       'andrew.j.dougherty'
      ],
     ],
    ];
  my $depthannotated =
    [
     2,
     'accounts',
     [
      1,
      'Gmail accounts',
      [
       0,
       'adougher9'
      ],
      [
       0,
       'andrew.j.dougherty'
      ],
     ],
    ];
  my $desired =
    {
     'accounts' => {
		    'Gmail accounts' => {
					 'adougher9' => 1,
					 'andrew.j.dougherty' => 1,
					},
		   },
    };
  my $res1 = AddDepthInformation
    (Item => $sample);
  print Dumper
    (ConvertNestedListsToHash
     (Item => $res1));
}
