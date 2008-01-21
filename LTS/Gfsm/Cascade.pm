## -*- Mode: CPerl -*-
## File: Lingua::LTS::Gfsm::Cascade.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description: Gfsm::XL::Cascade -based transductions

package Lingua::LTS::Gfsm::Cascade;
use Lingua::LTS::Gfsm;
use Gfsm;
use Gfsm::XL;
use Encode qw(encode decode);
use utf8;
use Tie::Cache;
use IO::File;
use Carp;

use strict;

##==============================================================================
## Globals
##==============================================================================

our $DEFAULT_CACHE_SIZE = $Lingua::LTS::Gfsm::DEFAULT_CACHE_SIZE;

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure:
##    (
##     ##-- Analysis objects
##     fst  => $cl,       ##-- a Gfsm::XL::Cascade::Lookup object (default=new)
##     lab  => $lab,      ##-- a Gfsm::Alphabet object (default=new)
##     labh => \%sym2lab, ##-- label hash
##     laba => \@lab2sym, ##-- label array
##     dict => \%dict,    ##-- exception dictionary
##     eow  => $str,      ##-- EOW string for analysis FST
##     result=>$resultfst, ##-- result fst
##
##     ##-- Lookup options (new)
##     max_paths  => $max_paths,  ##-- sets $cl->max_paths()
##     max_weight => $max_weight, ##-- sets $cl->max_weight()
##     max_ops    => $max_ops,    ##-- sets $cl->max_ops()
##
##     ##-- LRU Cache
##     cache => $tiedCache, ##-- uses Tie::Cache
##     cacheSize => $n,     ##-- cache size (default = ${__PACKAGE__."::DEFAULT_CACHE_SIZE"})
##
##     ##-- Options
##     check_symbols => $bool,  ##-- check for unknown symbols? (default=1)
##     labenc        => $enc,   ##-- encoding of labels file (default='latin1')
##
##     ##-- Profiling data
##     profile => $bool,     ##-- track profiling data (default=0)
##     ntoks   => $ntokens,  ##-- #/tokens processed
##     ndict   => $ndict,    ##-- #/dictionary-analyzed tokens
##     nknown  => $nknown,   ##-- #/known tokens (pre-, dict-, or fst-analyzed)
##
##     ntoksa  => $ntokensa, ##-- #/tokens processed (alphabetic)
##     ndicta  => $ndicta,   ##-- #/dictionary-analyzed tokens (alphabetic)
##     nknowna => $nknowna,  ##-- #/known tokens (pre-, dict-, or fst-analyzed) (alphabetic)
##
##     ##-- errors etc
##     errfh   => $fh,       ##-- FH for warnings/errors (default=STDERR; requires: "print()" method)
##    )
sub new {
  my $that = shift;
  my $lts = $that->SUPER::new({
			       ##-- analysis objects
			       fst=>Gfsm::XL::Cascade::Lookup->new(),

			       ##-- lookup options
			       max_weight => 1e38,
			       max_paths  => 1,
			       max_ops    => -1,

			       ##-- user args
			       @_
			      }, ref($that)||$that);
  $lts->setLookupOptions();
  return $lts;
}


## $lts = $lts->clear()
sub clear {
  my $lts = shift;

  $lts->{fst}->_cascade_set(undef);

  ##-- inherited
  $lts->SUPER::clear();
}

## $lts = $lts->resetProfilingData()
## - inherited

##--------------------------------------------------------------
## Methods: Lookup Options

## $lts = $lts->setLookupOptions(%opts)
## + %opts keys:
##   max_weight => $w,
##   max_paths  => $n_paths,
##   max_ops    => $n_ops,
sub setLookupOptions {
  my $lts  = shift;
  my %opts = (%$lts,@_);
  my $cl   = $lts->{fst};
  $cl->max_weight($opts{max_weight}) if (defined($opts{max_weight}));
  $cl->max_paths ($opts{max_paths}) if (defined($opts{max_paths}));
  $cl->max_ops   ($opts{max_ops}) if (defined($opts{max_ops}));
  return $lts;
}

##==============================================================================
## Methods: I/O
##==============================================================================

##--------------------------------------------------------------
## Methods: I/O: Input: all

## $lts = $lts->load(fst=>$fstFile, lab=>$labFile, dict=>$dictFile)
## + inherited

##--------------------------------------------------------------
## Methods: I/O: Input: Dictionary

## $lts = $lts->loadDict($dictfile)
## + inherited


##--------------------------------------------------------------
## Methods: I/O: Input: Transducer

## $lts = $lts->loadCascade($cscfile)
## $lts = $lts->loadFst    ($cscfile)
*loadFst = \&loadCascade;
sub loadCascade {
  my ($lts,$cscfile) = @_;
  my $csc = Gfsm::XL::Cascade->new();
  $csc->load($cscfile)
    or confess(ref($lts)."::loadCascade(): load failed for '$cscfile': $!");
  $lts->{fst}->cascade($csc);
  $lts->setLookupOptions();
  $lts->{result} = undef;  ##-- reset result automaton
  return $lts;
}

##--------------------------------------------------------------
## Methods: I/O: Input: Labels

## $lts = $lts->loadLabels($labfile)
## + inherited

## $lts = $lts->parseLabels()
## + inherited

##==============================================================================
## Methods: Analysis
##==============================================================================

## @analyses         = analyze($native_perl_word)
## $analysis_or_word = analyze($native_perl_word)
##  + inherited


1; ##-- be happy

__END__

##==============================================================================
## PODS
##==============================================================================

##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl

##========================================================================
## NAME
=pod

=head1 NAME

Lingua::LTS::Gfsm - Gfsm-based letter-to-sound transduction

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 ##========================================================================
 ## PRELIMINARIES

 use Lingua::LTS::Gfsm;

 ##========================================================================
 ## Constructors etc.

 $obj = CLASS_OR_OBJ->new(%args);    ##-- new object
 $lts = $lts->resetCache();          ##-- clear cached analyses
 $lts = $lts->clear();               ##-- clear entire object
 $lts = $lts->resetProfilingData();  ##-- clear profiling data

 ##========================================================================
 ## Methods: I/O

 $lts = $lts->load(fst  => $fstFile,
                   lab  => $labFile,
                   dict => $dictFile); ##-- load all analysis objects at once

 $lts = $lts->loadDict($dictfile);     ##-- load exception dictionary (optional)
 $lts = $lts->loadFst($fstfile);       ##-- load analysis transducer (required)
 $lts = $lts->loadLabels($labfile);    ##-- load analysis alphabet (required)

 $lts = $lts->parseLabels();           ##-- index loaded labels (low-level)

 ##========================================================================
 ## Methods: Analysis

 @analyses         = analyze($native_perl_word); ##-- non-deterministic analysis
 $analysis_or_word = analyze($native_perl_word)  ##-- (pesudo-)deterministic analysis

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::Gfsm: Globals
=pod

=head2 Globals

=over 4

=item Variable: $DEFAULT_CACHE_SIZE

Default cache size (number of analyses to store) for new Lingua::LTS::Gfsm objects.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::Gfsm: Constructors etc.
=pod

=head2 Constructors etc.

=over 4

=item new

 $obj = CLASS_OR_OBJ->new(%args);

object structure / keyword %args:

    (
     ##
     ##-- Analysis objects
     fst  => $gfst,     ##-- a Gfsm::Automaton object (default=new)
     lab  => $lab,      ##-- a Gfsm::Alphabet object (default=new)
     labh => \%sym2lab, ##-- label hash
     laba => \@lab2sym, ##-- label array
     dict => \%dict,    ##-- exception dictionary
     eow  => $str,      ##-- EOW string for analysis FST
     result=>$resultfst, ##-- result fst (temporary)
     ##
     ##-- LRU Cache
     cache => $tiedCache, ##-- uses Tie::Cache
     cacheSize => $n,     ##-- cache size (default = ${__PACKAGE__."::DEFAULT_CACHE_SIZE"})
     ##
     ##-- Options
     check_symbols => $bool,  ##-- check for unknown symbols? (default=1)
     labenc        => $enc,   ##-- encoding of labels file (default='latin1')
     ##
     ##-- Profiling data
     profile => $bool,     ##-- track profiling data (default=0)
     ntoks   => $ntokens,  ##-- #/tokens processed
     ndict   => $ndict,    ##-- #/dictionary-analyzed tokens
     nknown  => $nknown,   ##-- #/known tokens (pre-, dict-, or fst-analyzed)
     ntoksa  => $ntokensa, ##-- #/tokens processed (alphabetic)
     ndicta  => $ndicta,   ##-- #/dictionary-analyzed tokens (alphabetic)
     nknowna => $nknowna,  ##-- #/known tokens (pre-, dict-, or fst-analyzed) (alphabetic)
     ##
     ##-- errors etc
     errfh   => $fh,       ##-- FH for warnings/errors (default=STDERR; requires: "print()" method)
    )


=item resetCache

 $lts = $lts->resetCache();

Resets the internal analysis cache.

=item clear

 $lts = $lts->clear();

Clear entire object.

=item resetProfilingData

 $lts = $lts->resetProfilingData();

Clear profiling data.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::Gfsm: Methods: I/O
=pod

=head2 Methods: I/O

=over 4

=item load

 $lts = $lts->load(fst=>$fstFile, lab=>$labFile, dict=>$dictFile);

Wrapper for loadFst(), loadLabels(), and loadDict().

=item loadDict

 $lts = $lts->loadDict($dictfile);

Load an exception dictionary (optional).

=item loadFst

 $lts = $lts->loadFst($fstfile);

Load an analysis transducer (required).

=item loadLabels

 $lts = $lts->loadLabels($labfile);

Load an analysis alphabet (required).

=item parseLabels

 $lts = $lts->parseLabels();

Index loaded alphabet.
Implicitly called by loadLabels().
You should call this method after altering the loaded alphabet in any way,
and before analyzing any words.

Effect(s):

=over 4

=item *

sets up $lts-E<gt>{labh}, $lts-E<gt>{laba}

=item *

fixes encoding difficulties in $lts-E<gt>{labh}, $lts-E<gt>{laba}

=back

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::Gfsm: Methods: Analysis
=pod

=head2 Methods: Analysis

=over 4

=item analyze

 @analyses         = analyze($native_perl_word);
 $analysis_or_word = analyze($native_perl_word)

Perform non-deterministic (list context, first form) or pseudo-deterministic analysis
(scalar context, second form) of the string $native_perl_word.  The exception
dictionary (if any) is consulted first.

If no dictionary entry is found, the cache is consulted.  If no cached
result is available, then the word is passed through the analysis FST, using
(potentially wide) characters
as alphabet input symbols.  In scalar context, the first analysis found
is returned as a string of concatenated FST symbols, otherwise the
literal $native_perl_word is returned (yes, this is stupid and goofy, but that's
what it does).  In list context, a (potentially empty) list of all analyses is returned.

Implicitly applies
character-set encoding,
end-of-word marker insertion,
input symbol-checking
and collection of profiling data
as indicated by the Lingua::LTS::Gfsm object's internal flags.

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl
=pod



=cut

=cut

##======================================================================
## Footer
##======================================================================

=pod

=head1 AUTHOR

Bryan Jurish E<lt>moocow@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
