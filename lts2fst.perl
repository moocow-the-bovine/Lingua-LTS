#!/usr/bin/perl -w

use IO::File;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;
use File::Basename qw(basename);

use lib qw(.);
use Lingua::LTS;

##==============================================================================
## Constants & Globals
##==============================================================================


## LTS structure
##   $lts = { classes=>\%classes, rules=>\@rules, ... }
our $lts = Lingua::LTS->new();

our $outfile = '-';
our $symfile = undef; ##-- default: use `basename($outfile)`.sym

our $verbose = 1;

##==============================================================================
## Command-line
##==============================================================================
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- debugging
	   'verbose|v' => \$verbose,

	   ##-- Output
	   'output|o|lexfile|l=s' => \$outfile,
	   'symbols|symfile|s:s' => \$symfile,
	   'qlabels|statelabels|ql|q=s' => \$qlabfile,
	   'nosymbols|nosyms|ns|S' => sub { $symfile=''; },
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##==============================================================================
## Subs
##==============================================================================

##--------------------------------------------------------------
## helper: verbose operation

sub mainop {
  my ($msg,$sub) = @_;
  print STDERR "$0: $msg..." if ($verbose);
  &$sub();
  print STDERR " done.\n" if ($verbose);
}

##==============================================================================
## MAIN
##==============================================================================

push(@ARGV,'-') if (!@ARGV);
$lts_file = shift;
mainop("loading LTS file '$lts_file'", sub { $lts->load($lts_file) });

##-- add 'letters', 'phones', 'specials' keys
mainop("expanding alphabet", sub { $lts->expand_alphabet() });

##-- expand rules
mainop("expanding rules", sub { $lts->expand_rules() });

##-- generate automaton
#mainop("generating automaton", sub { $fst = $lts->toAutomaton() });
$fst = $lts->toAutomaton();

##-- symbols
if ($outfile ne '-' && !defined($symfile)) {
  $outbase = $outfile;
  $outbase =~ s/\.[^\.]*$//;
  $symfile = $outbase . ".sym";
}
##-- save symbols file (maybe)
mainop("saving symbols file '$symfile'", sub { $fst->save_symbols($symfile) })
  if ($symfile);

##-- state labels
mainop("saving state labels file '$qlabfile'", sub { $fst->save_state_labels($qlabfile) })
  if ($qlabfile);

##-- save tfst
mainop("saving AT&T automaton text file '$outfile'",
       sub {
	 $fst->save_att_text($outfile,use_state_labels=>defined($qlabfile))
       });


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts2fst.perl - convert an LTS ruleset to an AT&T text transducer

=head1 SYNOPSIS

 lts2fst.perl [OPTIONS] [LTS_FILE...]

 Options:
  -help

 Output:
  -output  TFSTFILE
  -qlabels QLABFILE                ##-- state labels
  -symbols SYMFILE  , -nosymbols   ##-- default is `basename RULFILE`.sym

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

