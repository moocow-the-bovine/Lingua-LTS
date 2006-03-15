## File: Lingua::LTS::Trie.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description: Aho-Corasick pattern matcher

package Lingua::LTS::ACPM;
use Lingua::LTS::Trie;
use strict;
use Carp;

our @ISA = qw(Lingua::LTS::Trie);

##==============================================================================
## Constants
##==============================================================================

our $VERSION = 0.01;

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + NOTE:
##     output values are assumed to be hashrefs where they are defined
##  + object structure:
##     ##-- new in Lingua::LTS::ACPM
##     fail    => \@fail,    ##-- [$qid] => $qid_to_fail
##     failstr => $str,      ##-- for Gfsm export: symbol for fail. (default='<fail>')
##     ##-- inherited from Lingua::LTS::Trie
##     goto  => \@delta,   ##-- [$qid]{$sym} => $qid_to     s.t. $qid --$sym--> $qid_to
##     rgoto => \%rdelta,  ##-- [$qid_to]    => "$qid $sym" s.t. $qid --$sym--> $qid_to
##     out   => \%output,  ##-- {$qid}       => $output_hashref
##     chars => \%chars,   ##-- {$char}      => undef
##     cw    => $symbol_width, ##-- scalar width of a single input symbol (default=1)
##     nq    => $nstates,      ##-- scalar: number of states (>= 1)
sub new {
  my $that = shift;
  my $acpm = $that->SUPER::new(
			       fail=>[],
			       failstr=>'<fail>',
			       @_
			      );
}

## $acpm = $class_or_obj->newFromTrie($lingua_lts_trie,%compile_args)
sub newFromTrie {
  my ($that,$trie,%compile_args) = @_;
  return $that->new()->fromTrie($trie,%compile_args);
}

##==============================================================================
## Methods: Construction
##==============================================================================

## $acpm = $acpm->fromTrie($lingua_lts_trie,%args)
##  + %args:
##     joinout=>\&sub ##-- $out1_NEW = &sub($out1_old,$out2)
##                    ##-- i.e. a union operation: if undefined, no output is joined
sub fromTrie {
  my ($acpm,$trie,%args) = @_;

  ##-- adopt trie
  my $tclone = $trie->clone();
  if (!ref($acpm)) {
    $acpm = ref($acpm)->new(%$tclone);
  } else {
    @$acpm{keys %$tclone} = values %$tclone;
  }

  return $acpm->compile(%args);
}


## $acpm = $acpm->compile(%args)
##  + %args:
##     joinout=>\&sub ##-- $out1_NEW = &sub($out1_old,$out2)
##                    ##-- i.e. a union operation: if undefined, no output is joined
sub compile {
  my ($acpm,%args) = @_;

  ##-- useful vars
  my ($goto,$fail,$out) = @$acpm{qw(goto fail out)};
  my @chars             = keys(%{$acpm->{chars}});
  my $joinout           = $args{joinout};
  my ($q,$a);

  ##-- Phase 0: complete goto(q0,\Sigma)
  $fail->[0] = 0;
  foreach $a (grep {!exists($goto->[0]{$_})} @chars) {
    $goto->[0]{$a} = 0;
  }

  ##-- Phase 1: breadth-first fail(q) completion: init
  my @fifo = qw();
  foreach $a (grep {$goto->[0]{$_} != 0} @chars) {
    $fail->[$goto->[0]{$a}] = 0;
    push(@fifo, $goto->[0]{$a});
  }

  ##-- Phase 1: breadth-first fail(q) completion: search
  my ($r,$u,$v,$goto_va);
  while (defined($r=shift(@fifo))) {
    if ($r != 0 && exists($out->{$fail->[$r]}) && defined($joinout)) {
      $out->{$r} = $joinout->(@$out{$r,$fail->[$r]});
    }
    foreach $a (grep {defined($goto->[$r]{$_})} @chars) {
      $u=$goto->[$r]{$a};
      push(@fifo,$u);
      $v = $fail->[$r];
      $v = $fail->[$v] while (!defined($goto_va=$goto->[$v]{$a}));
      $fail->[$u] = $goto->[$v]{$a};
    }
  }

  return $acpm;
}


## $acpm = $acpm->complete(%args)
##  + adds $goto links for all $fail arcs
##  + %args: (none)
sub complete {
  my $acpm = shift;

  my ($goto,$fail) = @$acpm{qw(goto fail)};
  my @chars        = keys(%{$acpm->{chars}});
  my ($q,$r,$goto_ra,$a);
  foreach $q (0..($acpm->{nq}-1)) {
    foreach $a (@chars) {
      $r = $q;
      $r = $fail->[$r] while (!defined($goto_ra=$goto->[$r]{$a}));
      $goto->[$q]{$a} = $goto_ra;
    }
  }

  ##-- remove all fail links
  @{$acpm->{fail}} = qw();

  return $acpm;
}

##==============================================================================
## Methods: Lookup
##==============================================================================

## $q = $acpm->s2q($str)
sub s2q {
  my ($acpm,$str) = @_;
  my ($q,$qnext,$i,$a);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$acpm->{cw}) {
    $a = substr($str,$i,$acpm->{cw});
    if (exists($acpm->{chars}{$a})) { ##-- sanity check
      $q = $acpm->{fail}[$q] while (!defined($qnext=$acpm->{goto}[$q]{$a}));
      $q = $qnext;
    } else {
      $q = 0;
    }
  }
  return $q;
}

## \@states = $acpm->s2path($str)
sub s2path {
  my ($acpm,$str) = @_;
  my ($i,$q,$qnext,$a,@path);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$acpm->{cw}) {
    push(@path,$q);
    $a = substr($str,$i,$acpm->{cw});
    if (exists($acpm->{chars}{$a})) { ##-- sanity check
      $q = $acpm->{fail}[$q] while (!defined($qnext=$acpm->{goto}[$q]{$a}));
      $q = $qnext;
    } else {
      $q = 0;
    }
  }
  push(@path,$q) if (defined($q));
  return \@path;
}


## $str = $acpm->q2s($q);
#-> INHERITED from Trie

## $out = $acpm->q2out($q)
#-> INHERITED from Trie

## $out = $acpm->s2out($str)
#-> INHERITED from Trie

## ($q,$len) = $trie->s2prefixQ($str)
## $q        = $trie->s2prefixQ($str)
#-> INHERITED from Trie (goto only)

## $prefix = $trie->s2prefix($str)
#-> INHERITED from Trie (goto only)

##==============================================================================
## Methods: Full Match
##==============================================================================

## @outputs  = $acpm->matches($str)  ##-- list context
## \@outputs = $apcm->matches($str)  ##-- scalar context
sub matches {
  my @outputs = @{$_[0]{out}}{@{$_[0]->s2path($_[1])}};
  return wantarray ? @outputs : \@outputs;
}

##==============================================================================
## Methods: Export: Gfsm
##==============================================================================

## $labs = $acpm->gfsmInputLabels()
## $labs = $acpm->gfsmInputLabels($labs)
sub gfsmInputLabels {
  my ($acpm,$labs) = @_;
  $labs = $acpm->SUPER::gfsmInputLabels($labs);
  $labs->insert($acpm->{failstr});
  return $labs;
}

## $olabs = $trie->gfsmOutputLabels()
## $olabs = $trie->gfsmOutputLabels($labs,%args)
##  + output alphabet
##  + %args:
##     out2str => \&$sub #-- get key for output; default none (id)
#->inherited

## $labs = $acpm->gfsmStateLabels(?$labs,%args)
##  + state alphabet
##  + %args:
##     out2str => \&sub ##-- called as $sub->($out_or_undef) : default: stringify
#->inherited

## $gfsmDFA = $acpm->gfsmAutomaton(%args)
##  + %args:
##     fsm=>$fsm,            ##-- output automaton
##     ilabels =>$inLabels,  ##-- default: $trie->gfsmInputLabels()
##     dosort=>$bool,        ##-- sort automaton? (default=yes)
sub gfsmAutomaton {
  my ($acpm,%args) = @_;
  my $ilabs = defined($args{ilabels})  ? $args{ilabels} : $acpm->gfsmInputLabels();
  my $fsm   = defined($args{fsm})      ? $args{fsm}     : Gfsm::Automaton->newTrie();
  $fsm->is_transducer(0);
  $fsm->is_weighted(0);
  $fsm->root(0);
  my ($q,$qah,$a,$alab,$qto);
  my $faillab = $ilabs->get_label($acpm->{failstr});
  foreach $q (0..($acpm->{nq}-1)) {
    ##-- goto
    if (defined($qah = $acpm->{goto}[$q])) {
      while (($a,$qto)=each(%$qah)) {
	$alab = $ilabs->get_label($a);
	$fsm->add_arc($q,$qto, $alab,$alab, 0);
      }
    }
    ##-- fail
    if (defined($qto = $acpm->{fail}[$q])) {
      $fsm->add_arc($q,$qto, $faillab,$faillab, 0);
    }
    ##-- output
    if (exists($acpm->{out}{$q})) {
      $fsm->is_final($q,1);
    }
  }
  $fsm->arcsort(Gfsm::ASMLower()) if ($args{dosort} || !exists($args{dosort}));
  return $fsm;
}

## $gfsmFST = $trie->gfsmTransducer(%args)
##  + %args:
##     fsm=>$fsm,            ##-- output automaton
##     ilabels =>$inLabels,  ##-- default: $trie->gfsmInputLabels()
##     olabels =>$outLabels, ##-- default: $trie->gfsmOutputLabels()
##     dosort=>$bool,        ##-- sort automaton? (default=yes)
#-> INHERITED from Trie


##==============================================================================
## Methods: Debug: View
##==============================================================================

## undef = $acpm->viewps(%options)
#->inherited

## undef = $acpm->viewfst(%options)
#->inherited

1;

__END__

##==============================================================================
## PODS
##==============================================================================
=pod

=head1 NAME

Lingua::LTS::Trie - deterministic prefix tree map

=head1 SYNOPSIS

 use Lingua::LTS::Trie

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
