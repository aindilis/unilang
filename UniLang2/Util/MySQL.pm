package UniLang2::Util::MySQL;

use Data::Dumper;
use DBI;
use XML::Twig;

# actually this should be part of the PerlLib source manager system

# perhaps time to do some fresh rewrites of these systems

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / DBName DBH MyTwig /

  ];

sub init {
  my ($self,%args) = @_;
  $self->DBName($args{DBName} ||
		"UniLang");
  $self->DBH
    (DBI->connect
     ("DBI:mysql:database=" .
      "unilang".
      ";host=localhost",
      "root", "",
      {
       'RaiseError' => 1}));
}

my $months = {
	      "Jan" => 1,
	      "Feb" => 2,
	      "Mar" => 3,
	      "Apr" => 4,
	      "May" => 5,
	      "Jun" => 6,
	      "Jul" => 7,
	      "Aug" => 8,
	      "Sep" => 9,
	      "Oct" => 10,
	      "Nov" => 11,
	      "Dec" => 12,
	     };

sub LoadLogFileIntoDB {
  my ($self,%args) = @_;
  $self->MyTwig(XML::Twig->new
		(
		 twig_handlers =>
		 {
		  # message   => sub { print Dumper($_) },
		  # message   => sub { print ".\n" },
		  message   => sub {
		    my @values;
		    my $i = 1;
		    foreach my $key (qw(Sender Receiver Date Contents)) {
		      if ($key eq "Date") {
			my $date = $_->child_text($i);
			if ($date =~ /^(\w+)\s+(\w+)\s+([0-9]+)\s+([0-9:]+)\s+(\w+)\s+([0-9]+)$/) {
			  # Wed Dec 29 15:33:10 EST 2004
			  my $newdate = sprintf("%04i-%02i-%02i %s",$6,$months->{$2},$3,$4);
			  push @values, $newdate;
			} else {
			  print "error\n";
			}
		      } else {
			push @values, $_->child_text($i);
		      }
		      ++$i;
		    }
		    my $c = "insert into messages values (NULL,".
		      join(", ", map $self->DBH->quote($_), @values).
			");";
		    # print $c."\n";
		    $self->DBH->do($c);
		  },
		 },
		 pretty_print => 'indented', # output will be nicely formatted
		));
  $self->MyTwig->parsefile($args{LogFile});
  my_process($self->MyTwig);
}

1;
