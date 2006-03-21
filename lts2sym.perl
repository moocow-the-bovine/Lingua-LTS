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
our $isymfile = undef;
our (@symLetters,@symPhons,@symSpecials);

our $verbose = 1;

our $oLetter = undef;
our $oPhon   = undef;
our $oSpecial = undef;
our $oClassPrefix = undef;

##==============================================================================
## Command-line
##==============================================================================
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- debugging
	   'verbose|v' => \$verbose,

	   ##-- Symbols (input)
	   'isymbols|isymfile|is=s' => \$isymfile,
	   'iletter|il=s@' => \@symLetters,
	   'iphon|ip=s@'   => \@symPhons,
	   'ispecial|iS=s@'=> \@symSpecials,

	   ##-- Output
	   'oletter|ol=s' => \$oLetter,
	   'ophon|op=s'   => \$oPhon,
	   'ospecial|oS=s'=> \$oSpecial,
	   'oclass-prefix|oclass|oprefix|oc=s' => \$oClassPrefix,
	   'osymbols|output|o=s'               => \$outfile,

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

##-- load symbols file (if given)
mainop("loading symbols file '$isymfile'...",
       sub {
	 $lts->load_symbols($isymfile,
			    Letter  => (@symLetters  ? \@symLetters  : undef),
			    Phon    => (@symPhons    ? \@symPhons    : undef),
			    Special => (@symSpecials ? \@symSpecials : undef),
			   );
       },
      )
  if ($isymfile);

##-- add 'letters', 'phones', 'specials' keys
mainop("expanding alphabet... ",
       sub { $lts->expand_alphabet() },
       ''
      );
print STDERR
  (#"$0: -> ",
   scalar(keys %{$lts->{specials}}), " specials, ",
   scalar(keys %{$lts->{letters}}), " letters, ",
   scalar(keys %{$lts->{phones}}), " phones.\n");

##-- save symbols
mainop("saving symbols file '$outfile'...",
       sub {
	 $lts->save_symbols($outfile,
			    Letter => $oLetter,
			    Phon   => $oPhon,
			    Special=> $oSpecial,
			    ClassPrefix => $oClassPrefix,
			   );
       });


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts2sym.perl - convert an LTS ruleset to an AT&T symbols file

=head1 SYNOPSIS

 lts2sym.perl [OPTIONS] [LTS_FILE...]

 Options:
  -help

 Input:
  -isymbols      INPUT_SYMFILE
  -iletter       CLASSNAME
  -iphon         CLASSNAME
  -ispecial      CLASSNAME

 Output:
  -osymbols      OUTPUT_SYMFILE
  -oletter       CLASSNAME
  -ophon         CLASSNAME
  -ospecial      CLASSNAME
  -oclass-prefix LTS_CLASS_PREFIX


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

