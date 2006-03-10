#!/usr/bin/perl -w

use IO::File;
use Getopt::Long (':config'=>'no_ignore_case');
use Encode qw(encode decode);
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
our $do_index = 0;
our $do_lower = 1;
our $verbose = 0;
our $encoding = 'latin1';
our $outfile = '-';


##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- behavior
	   'words|w!' => \$input_words,
	   'verbose|v!' => \$lts->{apply_verbose},
	   'warn|W!'   => \$lts->{apply_warn},
	   'expand|x!'  => \$do_expand,
	   'index|i!'   => \$do_index,
	   'lower|l!'   => \$do_lower,
	   'encoding|e=s' => \$encoding,

	   'output|out|o=s' => \$outfile,

	   'bos!' => \$lts->{implicit_bos},
	   'eos!' => \$lts->{implicit_eos},
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
  my $uword = defined($encoding) ? decode($encoding,$word) : $word;
  $uword = lc($uword) if ($do_lower);
  #$uword = encode($encoding,$uword) if (defined($encoding));
  my @phones = $do_index ? $lts->apply_word_indexed($uword): $lts->apply_word($uword);
  $outfh->print($word, "\t", join(' ', @phones), "\n");
}



##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

$lts_file = shift;
$lts->load($lts_file);

$outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");

if ($do_expand || $do_index,) {
  print STDERR "$0: expanding alphabet... ";
  $lts->expand_alphabet;
  print STDERR "done.\n";

  print STDERR "$0: expanding ", scalar(@{$lts->{rules}}), " rules... ";
  $lts->expand_rules();
  print STDERR "expanded to ", scalar(@{$lts->{rulex}}), " rules.\n";

  if (!$do_index) {
    ##-- HACK: verbose expanded application
    $lts->{rules} = $lts->{rulex};
    %{$lts->{classes}} = qw();
  }

  if ($do_index) {
    print STDERR "$0: compiling ACPM index... ";
    $lts->compile_acpm();
    print STDERR "compiled $lts->{acpm}{nq} states.\n";
  }
}

push(@ARGV,'-') if (!@ARGV);
if ($input_words) {
  lts_apply_word($lts,$_) foreach (@ARGV);
} else {
  while (<>) {
    chomp;
    lts_apply_word($lts,$_);
  }
}

$outfh->close;


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
  -expand                # expand classes before applying rules (SLOW!)
  -index                 # use trie index to apply rules
  -tolower, -nolower     # canonicalize input to lower case
  -verbose               # trace rule application to STDERR
  -nowarn                # don't warn about untranslatable words
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

