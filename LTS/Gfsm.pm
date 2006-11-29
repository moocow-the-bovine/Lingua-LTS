## -*- Mode: CPerl -*-
## File: Lingua::LTS::Gfsm.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description: Gfsm-based letter-to-sound transduction

package Lingua::LTS::Gfsm;
use Gfsm;
use Encode qw(encode decode);
use utf8;
use Tie::Cache;
use IO::File;
use Carp;

use strict;

##==============================================================================
## Globals
##==============================================================================

our $DEFAULT_CACHE_SIZE = 2048;

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure:
##    (
##     ##-- Analysis objects
##     fst  => $gfst,     ##-- a Gfsm::Automaton object (default=new)
##     lab  => $lab,      ##-- a Gfsm::Alphabet object (default=new)
##     labh => \%sym2lab, ##-- label hash
##     laba => \@lab2sym, ##-- label array
##     dict => \%dict,    ##-- exception dictionary
##     eow  => $str,      ##-- EOW string for analysis FST
##     result=>$resultfst, ##-- result fst
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
  my $lts = bless({
		   ##-- analysis objects
		   fst=>Gfsm::Automaton->new,
		   lab=>Gfsm::Alphabet->new,
		   result=>Gfsm::Automaton->new,
		   labh=>{},
		   laba=>[],
		   dict=>{},
		   eow=>'',

		   ##-- cache options
		   cache=>{},
		   cacheSize=>$DEFAULT_CACHE_SIZE,

		   ##-- options
		   check_symbols => 1,
		   labenc        => 'latin1',

		   ##-- profiling
		   profile => 0,

		   ntoks   => 0,
		   ndict   => 0,
		   nknown  => 0,
		   ncache  => 0,

		   ntoksa  => 0,
		   ndicta  => 0,
		   nknowna => 0,
		   ncachea  => 0,

		   ##-- errors
		   errfh   => \*STDERR,

		   ##-- user args
		   @_
		  }, ref($that)||$that);
  $lts->resetCache();
  return $lts;
}

## $lts = $lts->resetCache()
##  + resets cache
sub resetCache {
  my $lts = shift;
  %{$lts->{cache}} = qw();
  if ($lts->{cacheSize} > 0) {
    tie(%{$lts->{cache}}, 'Tie::Cache', { MaxCount=>$lts->{cacheSize} })
      or confess(ref($lts)."::resetCache(): could not create cache!");
  } else {
    $lts->{cache} = {};
  }
  return $lts;
}

## $lts = $lts->clear()
sub clear {
  my $lts = shift;

  ##-- analysis objects
  $lts->{fst}->clear;
  $lts->{lab}->clear;
  $lts->{result}->clear if ($lts->{result});
  %{$lts->{labh}} = qw();
  @{$lts->{laba}} = qw();
  %{$lts->{dict}} = qw();

  ##-- cache
  %{$lts->{cache}} = qw();

  ##-- profiling
  return $lts->resetProfilingData();
}

## $lts = $lts->resetProfilingData()
sub resetProfilingData {
  my $lts = shift;
  $lts->{profile} = 0;
  $lts->{ntoks} = 0;
  $lts->{ndict} = 0;
  $lts->{nknown} = 0;
  $lts->{ntoksa} = 0;
  $lts->{ndicta} = 0;
  $lts->{nknowna} = 0;
  $lts->{ncache} = 0;
  $lts->{ncachea} = 0;
  return $lts;
}

##==============================================================================
## Methods: I/O
##==============================================================================

##--------------------------------------------------------------
## Methods: I/O: Input: all

## $lts = $lts->load(fst=>$fstFile, lab=>$labFile, dict=>$dictFile)
sub load {
  my ($lts,%args) = @_;
  my $rc = $lts;
  $rc &&= $lts->loadFst($args{fst}) if (defined($args{fst}));
  $rc &&= $lts->loadLabels($args{lab}) if (defined($args{lab}));
  $rc &&= $lts->loadDict($args{dict}) if (defined($args{dict}));
  return $rc;
}

##--------------------------------------------------------------
## Methods: I/O: Input: Dictionary

## $lts = $lts->loadDict($dictfile)
sub loadDict {
  my ($lts,$dictfile) = @_;
  my $dictfh = IO::File->new("<$dictfile")
    or confess(ref($lts),"::loadDict() open failed for dictionary file '$dictfile': $!");

  my $dict = $lts->{dict};
  my ($line,$word,$phones);
  while (defined($line=<$dictfh>)) {
    chomp($line);
    next if ($line =~ /^\s*$/ || $line =~ /^\s*%/);
    $line = decode($lts->{labenc}, $line) if ($lts->{labenc});
    $word = lc($word) if ($lts->{tolower});
    ($word,$phones) = split(/\t+/,$line,2);
    $dict->{$word} = $phones;
  }

  $dictfh->close;
  return $lts;
}


##--------------------------------------------------------------
## Methods: I/O: Input: Transducer

## $lts = $lts->loadFst($fstfile)
sub loadFst {
  my ($lts,$fstfile) = @_;
  $lts->{fst}->load($fstfile)
    or confess(ref($lts)."::loadFst(): load failed for '$fstfile': $!");
  $lts->{result} = $lts->{fst}->shadow;
  return $lts;
}

##--------------------------------------------------------------
## Methods: I/O: Input: Labels

## $lts = $lts->loadLabels($labfile)
sub loadLabels {
  my ($lts,$labfile) = @_;
  $lts->{lab}->load($labfile)
    or confess(ref($lts)."::loadLabels(): load failed for '$labfile': $!");
  $lts->parseLabels();
  return $lts;
}

## $lts = $lts->parseLabels()
##  + sets up $lts->{labh}, $lts->{laba}
##  + fixes encoding difficulties in $lts->{labh},$lts->{laba}
sub parseLabels {
  my $lts = shift;
  my $laba = $lts->{laba};
  @$laba = @{$lts->{lab}->asArray};
  my ($i);
  foreach $i (grep { defined($laba->[$_]) } 0..$#$laba) {
    $laba->[$i] = decode($lts->{labenc}, $laba->[$i]) if ($lts->{labenc});
    $lts->{labh}{$laba->[$i]} = $i;
  }
  return $lts;
}

##==============================================================================
## Methods: Analysis
##==============================================================================

## @analyses         = analyze($native_perl_word)
## $analysis_or_word = analyze($native_perl_word)
sub analyze {
  my ($lts,$word) = @_;
  my $uword = $lts->{tolower} ? lc($word) : $word;

  my $isalpha = $lts->{profile} && $word !~ /[^[:alpha:]]/;
  ++$lts->{ntoks} if ($lts->{profile});
  ++$lts->{ntoksa} if ($isalpha);

  ##-- dictionary check
  if (exists($lts->{dict}{$uword})) {
    ++$lts->{ndict};
    ++$lts->{ndicta} if ($isalpha);
    return $lts->{dict}{$uword};
  }

  my ($analyses);
  if (defined($analyses=$lts->{cache}{$uword})) {
    ##-- cache check
    ++$lts->{ncache};
    ++$lts->{ncachea} if ($isalpha);
  }
  else {
    ##-- FST lookup
    my @labs = @{$lts->{labh}}{split(//, $uword.$lts->{eow})};

    ##-- verbose symbol check
    if ($lts->{check_symbols}) {
      foreach (grep { !defined($labs[$_]) } (0..$#labs)) {
	$lts->{errfh}->print(ref($lts),
			     ": Warning: ignoring unknown character '",
			     substr($word,$_,1),
			     "' in word '$word'.\n",
			    );
      }
    }
    @labs = grep { defined($_) } @labs;

    ##-- lookup
    $lts->{fst}->lookup(\@labs,$lts->{result});
    $analyses =
      [
       grep { $_ ne '' }
       map {
	 join('',
	      map {
		length($lts->{laba}[$_]) > 1 ? "[$lts->{laba}[$_]]" : $lts->{laba}[$_]
	      } @{$_->{hi}}
	     )
       } @{$lts->{result}->paths($Gfsm::LSUpper)}
      ];

    ##-- cache these analyses
    $lts->{cache}{$uword} = $analyses if ($lts->{cacheSize});
  }

  if ($lts->{profile} && @$analyses) {
    ++$lts->{nknown};
    ++$lts->{nknowna} if ($isalpha);
  }

  return (wantarray
	  ? @$analyses
	  : (@$analyses
	     ? $analyses->[0]
	     : $uword));
}


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
