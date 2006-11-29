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

##--------------------------------------------------------------
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

##--------------------------------------------------------------
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

##--------------------------------------------------------------
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
## Methods: Class-Expansion
##==============================================================================

## $acpm = $acpm->expand($acpm, \%classes, %args)
## + requires:
##    - $acpm->complete()
##    - Gfsm
## + %classes maps ACPM class-symbols to pseudo-sets (keys) of literal symbols
## + %args:
##    packas  => $template_char  ##-- either 'S' or 'L': default='L'
##    joinout => \&sub,          ##-- as for compile()
sub expand {
  my ($acpm,$classes,%args) = @_;
  #require Gfsm;

  ##-- Phase 1: Generate NFA by expanding $classes
  my $goto    = $acpm->{goto};
  my $out     = $acpm->{out};
  my $joinout = $args{joinout};
  my $nfa     = Gfsm::Automaton->new();
  $nfa->is_deterministic(0);
  $nfa->is_transducer(0);
  $nfa->is_weighted(0);
  $nfa->sort_mode(Gfsm::ASMNone());
  $nfa->root(0);

  my ($q,$c,$cc,$clab,$qto);
  my $labs = Gfsm::Alphabet->new();
  $labs->insert('<eps>',0);
  foreach $q (0..($acpm->{nq}-1)) {
    $nfa->is_final($q,1) if (exists($out->{$q}));
    while (($c,$qto)=each(%{$goto->[$q]})) {
      if (exists($classes->{$c})) {
	##-- expand class
	foreach $cc (keys(%{$classes->{$c}})) {
	  $clab = $labs->get_label($cc);
	  $nfa->add_arc($q,$qto, $clab,$clab, 0);
	}
      }
      else {
	##-- literal arc
	$clab = $labs->get_label($c);
	$nfa->add_arc($q,$qto, $clab,$clab, 0);
      }
    }
  }

  ##-- Phase 2: Determinize NFA (native perl)
  my $laba   = $labs->asArray;
  my $packas = ($args{packas} ? $args{packas} : 'L').'*';
  my $q0     = pack($packas,0);
  my $id2set = $acpm->{id2set} = [$q0];
  my $set2id = $acpm->{set2id} = {$q0=>0};
  my $dgoto  = [];
  my $drgoto = [];
  my $dout   = {};
  my $ai     = Gfsm::ArcIter->new();

  my ($dq,@nqs,$nq,$nqh,$dqtop,$dqto, %c2nq);
  my @fifo = (0);
  while (defined($dq=shift(@fifo))) {
    @nqs = unpack($packas, $id2set->[$dq]);

    ##-- join output
    if (defined($joinout)) {
      foreach $nq (@nqs) {
	$dout->{$dq} = $joinout->($dout->{$dq}, $out->{$nq});
      }
    }

    ##-- get NFA transition map
    %c2nq = qw();
    foreach $nq (@nqs) {
      for ($ai->open($nfa,$nq); $ai->ok; $ai->next) {
	$c2nq{$laba->[$ai->lower]}{$ai->target} = undef;
      }
    }

    ##-- instantiate output states
    while (($c,$nqh)=each(%c2nq)) {
      if (!defined($dqto=$set2id->{$dqtop=pack($packas, sort {$a<=>$b} keys(%$nqh))})) {
	$dqto = $set2id->{$dqtop} = scalar(@$id2set);
	push(@$id2set, $dqtop);
	push(@fifo,    $dqto);
      }
      $dgoto->[$dq]{$c} = $dqto;
      $drgoto->[$dqto]  = "-1 $c"; ##-- HACK: rgoto is buggy in determinized ACPM
    }
  }

  ##-- Phase 3: instantiate the ACPM from the DFA
  $acpm->{goto}    = $dgoto;
  $acpm->{rgoto}   = $drgoto;
  $acpm->{out}     = $dout;
  $acpm->{nq}      = scalar(@$id2set);
  @{$acpm->{fail}} = qw();
  delete(@{$acpm->{chars}}{keys(%$classes)});

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
#-> BAD for expanded ACPM

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
## $labs = $acpm->gfsmInputLabels($labs,%args)
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
  $fsm->sort_mode(Gfsm::ASMNone()) if (defined($args{dosort}) && !$args{dosort});
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

##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl
=pod

=cut

##========================================================================
## NAME
=pod

=head1 NAME

Lingua::LTS::ACPM - native Perl Aho-Corasick pattern matcher object

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 ##========================================================================
 ## PRELIMINARIES

 use Lingua::LTS::Trie;
 use Lingua::LTS::ACPM;

 ##========================================================================
 ## Constructors etc.

 $obj = CLASS_OR_OBJ->new(%args);
 $acpm = $class_or_obj->newFromTrie($lingua_lts_trie,%compile_args);

 ##========================================================================
 ## Methods: Construction

 $acpm = $acpm->fromTrie($lingua_lts_trie,%args);
 $acpm = $acpm->compile(%args);
 $acpm = $acpm->complete(%args);

 ##========================================================================
 ## Methods: Class-Expansion

 $acpm = $acpm->expand($acpm, \%classes, %args);

 ##========================================================================
 ## Methods: Lookup

 $q = $acpm->s2q($str);
 \@states = $acpm->s2path($str);

 ##========================================================================
 ## Methods: Full Match

 @outputs  = $acpm->matches($str)  ##-- list context;

 ##========================================================================
 ## Methods: Export: Gfsm

 $labs = $acpm->gfsmInputLabels();
 $gfsmDFA = $acpm->gfsmAutomaton(%args);

 ##========================================================================
 ## Methods: Inherited

 #... any Lingua::LTS::Trie method ...

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Constructors etc.
=pod

=head2 Constructors etc.

=over 4

=item new

 $obj = CLASS_OR_OBJ->new(%args);

Creates and returns a new ACPM object.
Output values (in $args{out}) are assumed to be hashrefs where they are defined.

Object structure / keyword %args:

 ##-- inherited from Lingua::LTS::Trie
 goto  => \@delta,   ##-- [$qid]{$sym} => $qid_to     s.t. $qid --$sym--E<gt> $qid_to
 rgoto => \%rdelta,  ##-- [$qid_to]    => "$qid $sym" s.t. $qid --$sym--E<gt> $qid_to
 out   => \%output,  ##-- {$qid}       => $output_hashref
 chars => \%chars,   ##-- {$char}      => undef
 cw    => $symbol_width, ##-- scalar width of a single input symbol (default=1)
 nq    => $nstates,      ##-- scalar: number of states (E<gt>= 1)


=item newFromTrie

 $acpm = $class_or_obj->newFromTrie($lingua_lts_trie,%compile_args);

Creates and compiles new ACPM object from a Lingua::LTS::Trie object.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Methods: Construction
=pod

=head2 Methods: Construction

=over 4

=item fromTrie

 $acpm = $acpm->fromTrie($lingua_lts_trie,%args);

(Re-)initialize and compile an existing ACPM object from a Lingua::LTS::Trie.
%args are as for $acpm-E<gt>compile().


=item compile

 $acpm = $acpm->compile(%args);

Compile an ACPM object.  This method accepts an ACPM in trie-like format,
and completes its {goto} key, populates its {fail} key,
and updates its {out} key by the user-specified join callback (if any).

Recognized %args:

 joinout=>\&sub ##-- $out1_NEW = &sub($out1_old,$out2)
                ##-- i.e. a union operation: if undefined, no output is joined

=back

=item complete

 $acpm = $acpm->complete(%args);

Adds {goto} links for all {fail} arcs.

Currently does not recognize any %args at all.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Methods: Class-Expansion
=pod

=head2 Methods: Class-Expansion

=over 4

=item expand

 $acpm = $acpm->expand($acpm, \%classes, %args);

Expands class-labelled arcs in {acpm} to arcs labelled with literal terminal symbols
belonging to the respective classes.

%classes maps ACPM class-symbols to pseudo-sets (keys) of literal symbols.

Accepted %args:

 packas  => $template_char  ##-- either 'S' or 'L': default='L'
 joinout => \&sub,          ##-- as for compile()

Requires:

=over 4

=item *

A completed ACPM $acpm (see the complete() method).

=item *

The Gfsm package.

=back

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Methods: Lookup
=pod

=head2 Methods: Lookup

=over 4

=item s2q

 $q = $acpm->s2q($str);

Returns state achieved after following one arc for each character in $str.

=item s2path

 \@states = $acpm->s2path($str);

Returns state path induced by following one arc for each character in $str.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Methods: Full Match
=pod

=head2 Methods: Full Match

=over 4

=item matches

 @outputs = $acpm->matches($str)  ##-- list context;
 $outputs = $apcm->matches($str)  ##-- scalar context (ARRAY-ref)

Gathers output(s) produced by following one arc for each character
in $str.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: Lingua::LTS::ACPM: Methods: Export: Gfsm
=pod

=head2 Methods: Export: Gfsm

=over 4

=item gfsmInputLabels

 $labs = $acpm->gfsmInputLabels();
 $labs = $acpm->gfsmInputLabels($labs,%args)

Returns ACPM input labels as a Gfsm::Alphabet object.

=item gfsmAutomaton

 $gfsmDFA = $acpm->gfsmAutomaton(%args);

Returns ACPM as a Gfsm::Automaton object (recognizer).

Recognized %args:

 fsm     =>$fsm,       ##-- output automaton
 ilabels =>$inLabels,  ##-- default: $trie-E<gt>gfsmInputLabels()
 dosort  =>$bool,      ##-- sort automaton? (default=yes)

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

=cut


=cut

