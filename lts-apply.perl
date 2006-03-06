#!/usr/bin/perl -w

use IO::File;
use Getopt::Long;
use Pod::Usage;

use lib qw(.);
use Lingua::LTS;

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------

## LTS structure
##   $lts = { classes=>\%classes, rules=>\@rules, ... }
our $lts = Lingua::LTS->new();

our $input_words = 0;
our $do_expand = 0;
our $verbose = 0;


##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- behavior
	   'words|w!' => \$input_words,
	   'verbose|v!' => \$lts->{apply_verbose},

	   'bos|b!' => \$lts->{implicit_bos},
	   'eos|e!' => \$lts->{implicit_eos},
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help || !@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##--------------------------------------------------------------
## LTS: application + I/O

## undef = lts_apply_word($lts,$word)
##  + just prints out
sub lts_apply_word {
  my ($lts,$word) = @_;
  my @phones = $lts->apply_word($word);
  print $word, "\t", join(' ', @phones), "\n";
}



##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

$lts_file = shift;
$lts->load($lts_file);

push(@ARGV,'-') if (!@ARGV);
if ($input_words) {
  lts_apply_word($lts,$_) foreach (@ARGV);
} else {
  while (<>) {
    chomp;
    lts_apply_word($lts,$_);
  }
}


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts-apply.perl - apply LTS rules

=head1 SYNOPSIS

 lts-apply.perl [OPTIONS] LTS_FILE [INPUTS...]

 Options:
  -help
  -words                 # inputs are words, not filenames
  #-expand               # expand classes before applying rules (SLOW!)
  -verbose               # trace rule application to STDERR
  -bos , -nobos          # do/don't implicitly prepend word-initial '#' (default=yes)
  -eos , -noeos          # do/don't implicitly append  word-final   '#' (default=yes)

=cut

##------------------------------------------------------------------------------
## Options and Arguments
##------------------------------------------------------------------------------
=pod

=head1 OPTIONS AND ARGUMENTS

Not yet written.

=cut

##------------------------------------------------------------------------------
## Description
##------------------------------------------------------------------------------
=pod

=head1 DESCRIPTION

Not yet written.

=cut


##------------------------------------------------------------------------------
## Footer
##------------------------------------------------------------------------------
=pod

=head1 AUTHOR

Bryan Jurish E<lt>moocow@bbaw.deE<gt>

=cut

