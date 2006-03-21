#!/usr/bin/perl -w

use IO::File;
use Getopt::Long (':config'=>'no_ignore_case');
use Encode qw(encode decode);
use Time::HiRes qw(gettimeofday tv_interval);
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
our $do_sanitize = 0;
our $do_index = 0;
our $do_lower = 1;
our $verbose = 0;
our $encoding = 'latin1';
our $outfile = '-';

our $do_gindex = 0;

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
	   'sanitize|s!' => \$do_sanitize, ##-- sanitize rules?
	   'index|i!'   => \$do_index,
	   'gindex|g!'  => \$do_gindex, ##-- little bit faster: real dft ought to help ...

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
  my @phones = ($do_index && !$do_gindex
		? $lts->apply_word_indexed($uword)
		: ($do_gindex
		   ? $lts->apply_word_indexed_acpm_gfsm($uword)
		   : $lts->apply_word($uword)));
  $outfh->print($word, "\t", join(' ', @phones), "\n");
}



##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

$lts_file = shift;
$lts->load($lts_file);

$outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");


if ($do_expand || $do_index || $do_gindex || $do_sanitize) {
  print STDERR "$0: expanding alphabet... ";
  $lts->expand_alphabet;
  print STDERR "done.\n";
}

if ($do_sanitize) {
  my $nplain = scalar(@{$lts->{rules}});
  print STDERR "$0: sanitizing rules... ";
  $lts->sanitize_rules();
  print STDERR "added ", (scalar(@{$lts->{rules}})-$nplain), " default rules.\n";
}

if ($do_expand || $do_index || $do_gindex) {
  print STDERR "$0: expanding ", scalar(@{$lts->{rules}}), " rules... ";
  $lts->expand_rules();
  print STDERR "expanded to ", scalar(@{$lts->{rulex}}), " rules.\n";

  if (!$do_index && !$do_gindex) {
    ##-- HACK: verbose expanded application
    $lts->{rules} = $lts->{rulex};
    %{$lts->{classes}} = qw();
  }

  if ($do_index || $do_gindex) {
    print STDERR "$0: compiling ACPM index... ";
    $lts->compile_acpm();
    print STDERR "compiled $lts->{acpm}{nq} states.\n";

    if ($do_gindex) {
      require Gfsm;

      print STDERR "$0: completing ACPM... ";
      $lts->{acpm}->complete();
      print STDERR "done.\n";

      print STDERR "$0: generating GFSM index... ";
      $lts->{glabs} = $lts->{acpm}->gfsmInputLabels();
      $lts->{gacpm} = $lts->{acpm}->gfsmAutomaton(ilabels=>$lts->{glabs});
      print STDERR "done.\n";
    }
  }
}

my $tv_started = [gettimeofday];
my $ntoks = 0;

push(@ARGV,'-') if (!@ARGV);
if ($input_words) {
  lts_apply_word($lts,$_) foreach (@ARGV);
  $ntoks = @ARGV;
} else {
  print STDERR "$0: processing... ";
  while (<>) {
    chomp;
    lts_apply_word($lts,$_);
    ++$ntoks;
  }
  print STDERR "done.\n";
}

$outfh->close;

##-- summary
my $elapsed = tv_interval($tv_started, [gettimeofday]);
print STDERR
  sprintf("$0: processed %d tokens in %.2f secs: %.2f tok/sec\n",
	  $ntoks, $elapsed, ($elapsed ? $ntoks/$elapsed : -1));


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

