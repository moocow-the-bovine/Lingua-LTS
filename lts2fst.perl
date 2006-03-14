#!/usr/bin/perl -w

use IO::File;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;
use File::Basename qw(basename);

use lib qw(.);
use Lingua::LTS;
use Gfsm;

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
	   'output|o|F=s'        => \$outfile,
	   'symbols|symfile|s=s' => \$symfile,
	   'qlabels|statelabels|ql|q=s' => \$qlabfile,
	   'nosymbols|nosyms|ns|S' => sub { $symfile=''; }, ##-- defined but empty: no symbols

	   ##-- behavior
	   'bos|b!' => \$lts->{implicit_bos},
	   'eos|e!' => \$lts->{implicit_eos},
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##==============================================================================
## Subs
##==============================================================================

##--------------------------------------------------------------
## helper: verbose operation

sub mainop {
  my ($msg,$sub,$done) = @_;
  print STDERR "$0: $msg" if ($verbose);
  &$sub();
  print STDERR (defined($done) ? $done : " done.\n") if ($verbose);
}

##==============================================================================
## MAIN
##==============================================================================

push(@ARGV,'-') if (!@ARGV);
$lts_file = shift;
mainop("loading LTS file '$lts_file'...", sub { $lts->load($lts_file) });

##-- add 'letters', 'phones', 'specials' keys
mainop("expanding alphabet",
       sub { $lts->expand_alphabet() },
      );
print STDERR
  ("$0: -> ",
   scalar(keys %{$lts->{letters}}), " letters, ",
   scalar(keys %{$lts->{phones}}), " phones.\n");

##-- expand rules
mainop("expanding rules...",
       sub { $lts->expand_rules() },
      );
print STDERR
  ("$0: -> ",
   scalar(@{$lts->{rules}}), " aliased, ",
   scalar(@{$lts->{rulex}}), " expanded.\n");

##-- generate ACPM index
mainop("generating ACPM index...",
       sub { $acpm = $lts->toACPM(complete=>0); },
      );
print STDERR "$0: -> $acpm->{nq} states.\n";

##-- generate labels
mainop("generating I/O labels...", sub { $iolabs = $lts->gfsmLabels(); });

##-- generate automaton
mainop("generating FST...\n",
       sub { $fst = $lts->gfsmTransducer(ilabels=>$iolabs,olabels=>$iolabs,verbose=>1); },
       "$0: FST generated.\n",
      );

##-- symbols
if ($outfile ne '-' && !defined($symfile)) {
  $outbase = $outfile;
  $outbase =~ s/\.[^\.]*$//;
  $symfile = $outbase . ".sym";
}
##-- save symbols file (maybe)
mainop("saving symbols file '$symfile'...", sub { $lts->save_symbols($symfile) })
  if ($symfile);

##-- state labels
if ($qlabfile) {
  mainop("saving state labels file '$qlabfile'...",
	 sub {
	   $qlabs = $acpm->gfsmStateLabels(undef,out2str=>undef);
	   foreach $q ($qlabs->size()..($fst->n_states-1)) {
	     $qlabs->insert("q$q", $q);
	   }
	   $qlabs->save($qlabfile);
	 });
}


##-- save tfst
mainop("saving AT&T automaton text file '$outfile'...",
       sub {
	 $fst->print_att($outfile, lower=>$iolabs, upper=>$iolabs, states=>$qlabs);
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
  -symbols SYMFILE  , -nosymbols   ##-- default is `basename TFSTFILE`.sym

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

