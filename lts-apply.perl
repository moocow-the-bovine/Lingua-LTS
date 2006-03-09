#!/usr/bin/perl -w

use IO::File;
use Getopt::Long qw(:config no_ignore_case);
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
our $verbose = 0;


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
  my @phones = $do_index ? $lts->apply_word_indexed($word): $lts->apply_word($word);
  print $word, "\t", join(' ', @phones), "\n";
}



##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

$lts_file = shift;
$lts->load($lts_file);

if ($do_expand || $do_index,) {
  print STDERR "$0: expanding alphabet... ";
  $lts->expand_alphabet;
  print STDERR "done.\n";

  print STDERR "$0: expanding ", scalar(@{$lts->{rules}}), " rules... ";
  $lts->expand_rules();
  print STDERR "generated ", scalar(@{$lts->{rulex}}), " expanded rules.\n";

  if (!$do_index) {
    ##-- HACK
    $lts->{rules} = $lts->{rulex};
    %{$lts->{classes}} = qw();
  }

  if ($do_index) {
    print STDERR "$0: compiling trie index... ";
    $lts->compile_tries();
    print STDERR " done.\n";
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

