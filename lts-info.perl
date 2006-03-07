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

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- behavior
	   'expand|x!'  => \$do_expand,
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help || !@ARGV);


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

$lts_file = shift;
$lts->load($lts_file);
$lts->expand_alphabet();
$lts->expand_rules() if ($do_expand);

$info = $lts->info();
$vallen = 2;
$lablen = 19;
foreach (values(%$info)) { $vallen = length($_) if (length($_) > $vallen); }
print STDERR
  (sprintf("File: %s\n", $lts_file),
   sprintf(" + Alphabet:\n"),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Letters", $info->{alph_nLetters}),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Phones", $info->{alph_nPhones}),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Specials", $info->{alph_nSpecials}),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Classes", $info->{alph_nClasses}),
   sprintf(" + Rules:\n"),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Rules", $info->{nRules}),
   sprintf("   - %-${lablen}s: %${vallen}d\n", "#/Expanded Rules", ($do_expand ? $info->{nRulesX} : -1)),
   map {
     #sprintf("   - %-${lablen}s: %${vallen}d .. %${vallen}d\n",
     sprintf("   - %-${lablen}s: %${vallen}d .. %${vallen}d\n",
	     sprintf("min..max len(%-4s)", uc($_)),
	     $info->{"rule_min_len_$_"},
	     $info->{"rule_max_len_$_"},
	    )
   } qw(lhs in out rhs ltrs),
  );


__END__

##------------------------------------------------------------------------------
## PODS
##------------------------------------------------------------------------------
=pod

=head1 NAME

lts-info.perl - LTS ruleset information

=head1 SYNOPSIS

 lts-info.perl [OPTIONS] LTS_FILE

 Options:
  -help
  -expand                # show information on expanded classes

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

