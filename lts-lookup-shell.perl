#!/usr/bin/perl -w

use lib qw(.);
use Lingua::LTS;
use Lingua::LTS::Gfsm;
use Encode qw(encode decode);
use File::Basename qw(basename);
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;

##==============================================================================
## Constants & Globals
##==============================================================================

##-- program identity
our $prog = basename($0);
our $VERSION = 0.01;

##-- General Options
our ($help,$man,$version);

##-- FST Options
our $fstFile = undef;
our $labFile = undef;
our $dictFile = undef;
#our $deterministic = 0;

##-- Encoding Options
our $labEnc    = 'latin1'; ##-- no change
our $inputEnc  = undef;    ##-- defaults to $labEnc
our $outputEnc = undef;    ##-- no change

##-- Object
our $gfsm = Lingua::LTS::Gfsm->new();

##==============================================================================
## Command-line
##==============================================================================
GetOptions(##-- General
	   'help|h'    => \$help,
	   'man|m'     => \$man,
	   'version|V' => \$version,

	   ##-- FST Options
	   'fst|f=s' => \$fstFile,
	   'lab|l=s' => \$labFile,
	   'dict|d=s' => \$dictFile,
	   'eow|e=s'  => \$gfsm->{eow},
	   'weighted|weights|w!' => \$gfsm->{analyzeWeights},
	   'check-symbols|check|c!' => \$gfsm->{check_symbols},
	   'tolower|lower!' => \$gfsm->{tolower},
	   'tolowerNI|lowerNI!' => \$gfsm->{tolowerNI},
	   #'deterministic|D!' => \$deterministic,

	   ##-- Encoding Options
	   'label-encoding|lab-encoding|labenc|le=s' => \$labEnc,
	   'input-encoding|inenc|ie=s' => \$inputEnc,
	   'output-encoding|outenc|oe=s' => \$outputEnc,
	  );

pod2usage({-exitval=>0, -verbose=>1}) if ($man);
pod2usage({-exitval=>0, -verbose=>0}) if ($help);

if ($version) {
  print STDERR
    ("${prog} v$VERSION by Bryan Jurish <moocow\@bbaw.de>\n",
     "  + using Lingua::LTS v$Lingua::LTS::VERSION\n",
    );
  exit(0);
}
pod2usage({-exitval=>0, -verbose=>0, -msg=>"No FST specified!"})
  if (!defined($fstFile));


##==============================================================================
## Subs
##==============================================================================

sub analyze {
  my ($w) = shift;
  $w = decode($inputEnc,$w) if (defined($inputEnc));
  my @analyses = $gfsm->analyze($word);
  print
    (map { defined($outputEnc) ? encode($outputEnc,$_) : $_ }
     map { "\t$w : $_\n" }
     @analyses
    );
}

##==============================================================================
## MAIN
##==============================================================================

##-- Defaults
if (!defined($labFile)) {
  $labFile = $fstFile;
  $labFile =~ s/\.[^\.]*$/\.lab/;
}
$inputEnc  = $labEnc if (!defined($inputEnc));
$outputEnc = $inputEnc if (!defined($outputEnc));

##-- Report
print STDERR
  ("$prog Configuration:\n",
   " FILES:\n",
   "  fst          : $fstFile\n",
   "  lab          : $labFile\n",
   "  dict         : ", ($dictFile||'(none)'), "\n",
   " ENCODING OPTIONS:\n",
   "  labEnc       : '$gfsm->{labenc}'\n",
   "  inputEnc     : $inputEnc\n",
   "  outputEnc    : $outputEnc\n",
   " FST OPTIONS:\n",
   "  eow          : '$gfsm->{eow}'\n",
   "  check_symbols: $gfsm->{check_symbols}\n",
   "  tolower      : $gfsm->{tolower}\n",
   "  tolowerNI    : $gfsm->{tolowerNI}\n",
  );

##-- Load Automataon
print STDERR "$prog: loading automaton data...";
$gfsm->{labenc} = $labEnc;
$gfsm->load(fst=>$fstFile, lab=>$labFile, dict=>$dictFile)
  or die("$prog: failed to load automaton data: $!");

##-- Sanity check
die ("$prog: empty FST!")
  if (!$gfsm->{fst} || !$gfsm->{fst}->n_states);
print STDERR " loaded.\n";

##-- ye olde loope
$| = 1;
our $prompt = "<$prog> ";
print $prompt;
while (defined($word=<STDIN>)) {
  chomp($word);
  analyze($word);
  print $prompt;
}
print "Goodbye!\n";

__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts-lookup-shell.perl - debugging tool for gfsm lookup operations

=head1 SYNOPSIS

 lts-lookup-shell.perl [OPTIONS]

 General Options:
  -help          , -h
  -man           , -m
  -version       , -V

 FST Options:
  -fst  FSTFILE  , -f FSTFILE
  -lab  LABFILE  , -l LABFILE
  -dict DICTFILE , -d DICTFILE
  -eow  EOW_STR  , -e EOW_STR
  -weights       , -noweights
  -check         , -nocheck
  -tolower       , -nolower
  -tolowerNI     , -nolowerNI

 Encoding Options:
  -label-encoding ENCODING  , -le ENCODING
  -input-encoding ENCODING  , -ie ENCODING
  -output-encoding ENCODING , -oe ENCODING

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

