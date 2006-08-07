## -*- Mode: CPerl -*-
## File: Lingua::LTS::Gfsm.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: Gfsm-based letter-to-sound transduction

package Lingua::LTS::Gfsm;
use Gfsm;
use Encode qw(encode decode);
use utf8;
use IO::File;
use Carp;

use strict;

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
##     result=>$resultfst, ##-- result fst
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
##     errfh   => $fh,       ##-- FH for warnings/errors (default=STDERR)
##    )
sub new {
  my $that = shift;
  return bless({
		##-- analysis objects
		fst=>Gfsm::Automaton->new,
		lab=>Gfsm::Alphabet->new,
		result=>Gfsm::Automaton->new,
		labh=>{},
		laba=>[],
		dict=>{},

		##-- options
		check_symbols => 1,
		labenc        => 'latin1',

		##-- profiling
		profile => 0,
		ntoks   => 0,
		ndict   => 0,
		nknown  => 0,
		ntoksa  => 0,
		ndicta  => 0,
		nknowna => 0,

		##-- errors
		errfh   => \*STDERR,

		##-- user args
		@_
	       }, ref($that)||$that);
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
  return $lts;
}

##==============================================================================
## Methods: I/O
##==============================================================================

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

  ##-- exception check
  if (exists($lts->{dict}{$uword})) {
    ++$lts->{ndict};
    ++$lts->{ndicta} if ($isalpha);
    return $lts->{dict}{$uword};
  }

  ##-- FST lookup
  my @labs = @{$lts->{labh}}{split(//, $uword)};

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
  my @analyses =
    (
     grep { $_ ne '' }
     map {
       join('',
	    map {
	      length($lts->{laba}[$_]) > 1 ? "[$lts->{laba}[$_]]" : $lts->{laba}[$_]
	    } @{$_->{hi}}
	   )
     } @{$lts->{result}->paths($Gfsm::LSUpper)}
    );

  if ($lts->{profile} && @analyses) {
    ++$lts->{nknown};
    ++$lts->{nknowna} if ($isalpha);
  }

  return (wantarray
	  ? @analyses
	  : (@analyses
	     ? $analyses[0]
	     : $uword));
}


1; ##-- be happy

__END__

##==============================================================================
## PODS
##==============================================================================

=pod

=head1 NAME

Lingua::LTS - Festival-style letter-to-sound rules

=head1 SYNOPSIS

 use Lingua::LTS;

 #... stuff happens

=cut

##==============================================================================
## Description
##==============================================================================
=pod

=head1 DESCRIPTION

Not yet written.

=cut

##==============================================================================
## Methods
##==============================================================================
=pod

=head1 METHODS

Not yet written.

=cut


##==============================================================================
## Footer
##==============================================================================
=pod

=head1 AUTHOR

Bryan Jurish E<lt>moocow@bbaw.deE<gt>

=cut
