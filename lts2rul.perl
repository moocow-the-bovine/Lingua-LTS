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
	   'rverbose|rv|r' => \$lts->{apply_verbose},
	   'verbose|v' => \$verbose,

	   ##-- Output
	   'output|o|lexfile|l=s' => \$outfile,
	   'symbols|symfile|s:s' => \$symfile,
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


##-- expand all rules into $lts->{rulex}
#mainop("expanding rules", sub { $lts->expand_rules() });

##-- add 'letters', 'phones', 'specials' keys
mainop("expanding alphabet", sub { $lts->expand_alphabet() });

##-- symbols
if ($outfile ne '-' && !defined($symfile)) {
  $outbase = $outfile;
  $outbase =~ s/\.[^\.]*$//;
  $symfile = $outbase . ".sym";
}
##-- save symbols file (maybe)
mainop("saving symbols file '$symfile'",
       sub { $lts->save_symbols($symfile) })
  if ($symfile);

##-- lexicon
#mainop("generating lexicon file '$outfile", sub { $lts->to_lex($outfile) });

##-- cs rewrite rules
mainop("generating rewrite-rule file '$outfile'", sub { $lts->to_csrules($outfile) });


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts2rul.perl - convert Lingua::LTS rules to AT&T context-dependent rewrite rules [--BROKEN--]

=head1 SYNOPSIS

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !! BROKEN - DO NOT USE
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 #lts2rul.perl [OPTIONS] [LTS_FILE...]

 Options:
  -help

 Output:
  -output  RULFILE
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

