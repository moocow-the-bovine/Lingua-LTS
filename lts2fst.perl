#!/usr/bin/perl -w

use IO::File;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;
use File::Basename qw(basename);

use lib qw(.);
use Lingua::LTS;
use Lingua::LTS::Nd;
use Gfsm;

##==============================================================================
## Constants & Globals
##==============================================================================


## LTS structure
##   $lts = { classes=>\%classes, rules=>\@rules, ... }
our $lts = Lingua::LTS->new();

our $outfile = '-';
our $isymfile = undef;
our (@symLetters,@symPhons,@symSpecials,@symKeep);

our $verbose = 1;

##==============================================================================
## Command-line
##==============================================================================
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- debugging
	   'verbose|v' => \$verbose,

	   ##-- Symbols
	   'isymbols|isymfile|is=s' => \$isymfile,
	   'iletter|il=s@' => \@symLetters,
	   'iphon|ip=s@'   => \@symPhons,
	   'ispecial|iS=s@'=> \@symSpecials,
	   'ikeep|ik=s@'   => \@symKeep,

	   ##-- Output
	   'output|o|F=s'        => \$outfile,

	   ##-- behavior
	   'non-deterministic|nd' => sub { $lts = bless($lts,'Lingua::LTS::Nd'); },
	   'bos|b!' => \$lts->{implicit_bos},
	   'eos|e!' => \$lts->{implicit_eos},
	   'weight-rule|wr=f'       => \$lts->{weight_rule},
	   'weight-norule|wnr|wn=f' => \$lts->{weight_norule},
	   'weight-keep|wk=f'       => \$lts->{weight_keep},
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

##-- load symbols file (if given)
mainop("loading symbols file '$isymfile'...",
       sub {
	 $lts->load_symbols($isymfile,
			    Letter  => (@symLetters  ? \@symLetters  : undef),
			    Phon    => (@symPhons    ? \@symPhons   : undef),
			    Special => (@symSpecials ? \@symSpecials : undef),
			    Keep    => (@symKeep     ? \@symKeep   : undef),
			   );
       }
      )
  if ($isymfile);

##-- add 'letters', 'phones', 'specials' keys
mainop("expanding alphabet... ",
       sub { $lts->expand_alphabet() },
       ''
      );
print STDERR
  (#"$0: -> ",
   scalar(keys %{$lts->{specials}}), " special(s), ",
   scalar(keys %{$lts->{keep}}), " keeper(s), ",
   scalar(keys %{$lts->{letters}}), " letters, ",
   scalar(keys %{$lts->{phones}}), " phones.\n");

##-- sanitize rules
our $nvanilla = scalar(@{$lts->{rules}});
mainop("sanitizing rules... ",
       sub { $lts->sanitize_rules() },
       ''
      );
print STDERR
  (#"$0 -> ",
   "added ", scalar(@{$lts->{rules}})-$nvanilla, " default rule(s).\n",
  );

##-- expand rules
mainop("expanding rules... ",
       sub { $lts->expand_rules() },
       ''
      );
print STDERR
  (#"$0: -> ",
   scalar(@{$lts->{rules}}), " aliased, ",
   scalar(@{$lts->{rulex}}), " expanded.\n");

##-- generate labels
mainop("generating I/O labels...", sub { $iolabs = $lts->gfsmLabels(); });

##-- generate automaton
mainop("generating FST...\n",
       sub { $fst = $lts->gfsmTransducer(ilabels=>$iolabs,olabels=>$iolabs); },
       "$0: FST generated.\n",
      );

##-- save tfst
mainop("saving AT&T automaton text file '$outfile'...",
       sub {
	 $fst->print_att($outfile, lower=>$iolabs, upper=>$iolabs);
       });


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts2fst.perl - convert a Lingua::LTS ruleset to an AT&T text transducer

=head1 SYNOPSIS

 lts2fst.perl [OPTIONS] [LTS_FILE...]

 Options:
  -help

 Input Symbols:
  -isymbols SYMFILE
  -iletter  CLASSNAME
  -iphon    CLASSNAME
  -ispecial CLASSNAME

 LTS Configuration:
  -bos , -nobos
  -eos , -noeos
  -weight-rule  =WEIGHT  # weight for rule applications (default=0)
  -weight-keep  =WEIGHT  # weight for declared 'keep' symbols (default=0)
  -weight-norule=WEIGHT  # weight when no rules are applicable (default=0)
  -non-deterministic     # create non-deterministic weighted output (default=no)

 Output:
  -output  TFSTFILE

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

