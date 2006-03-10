## File: Lingua::LTS::Trie.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description: abstract tries


package Lingua::LTS::Trie;
use strict;
use Carp;

##==============================================================================
## Constants
##==============================================================================

our $VERSION = 0.01;

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure:
##     ##-- basic data
##     goto  => \@delta,   ##-- [$qid]{$sym} => $qid_to   s.t. $qid --$sym--> $qid_to
##     rgoto => \%rdelta,  ##-- [$qid_to]    => "$qid $sym" s.t. $qid --$sym--> $qid_to
##     out   => \%output,  ##-- {$qid}       => $output_scalar
##     chars => \%chars,   ##-- {$char}      => undef
##     cw    => $symbol_width,
##     nq    => $nstates,     ##-- scalar: number of states (>= 1)
##     epsilon => $epsilon_str, ##-- for gfsm label export
sub new {
  my $that = shift;
  return bless({
		goto=>[],
		rgoto=>[''],
		out=>{},
		chars=>{},
		nq=>1,
		cw=>1,
		epsilon=>'<eps>',
		@_
	       }, ref($that)||$that);
}

##==============================================================================
## Methods: clone
##==============================================================================

## $trie2 = $trie->clone
sub clone {
  my $trie = shift;
  require Storable;
  return Storable::dclone($trie);
}

##==============================================================================
## Methods: Construction
##==============================================================================

## $qid = $trie->add($str,$val)
sub add {
  my ($trie,$str,$val) = @_;
  my ($i,$q,$a,$qnext);
  for ($q=0,$i=0; $i < length($str); $q=$qnext, $i+=$trie->{cw}) {
    $a  = substr($str,$i,$trie->{cw});
    if (!defined($qnext=$trie->{goto}[$q]{$a})) {
      $qnext = $trie->{goto}[$q]{$a} = $trie->{nq}++;
      $trie->{rgoto}[$qnext] = $q.' '.$a;
      $trie->{chars}{$a} = undef;
    }
  }
  $trie->{out}{$q} = $val;
  return $q;
}

## undef = $trie->remove($str)
##  + hack: just removes output for $str
sub remove {
  my ($trie,$str) = @_;
  my $q = $trie->s2q($str);
  delete($trie->{out}{$q}) if (defined($q));
}

##==============================================================================
## Methods: Lookup
##==============================================================================

## $q = $trie->s2q($str)
sub s2q {
  my ($trie,$str) = @_;
  my ($q,$i);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$trie->{cw}) {
    $q = $trie->{goto}[$q]{substr($str,$i,$trie->{cw})};
  }
  return $q;
}

## $str = $trie->q2s($q);
sub q2s {
  my ($trie,$q) = @_;
  my @syms = qw();
  my ($qa,$a);
  while (defined($q) && $q > 0 && defined($qa=$trie->{rgoto}[$q])) {
    ($q,$a) = split(/ /, $qa, 2);
    push(@syms, $a);
  }
  return join('',reverse(@syms));
}

## \@states = $trie->s2path($str)
sub s2path {
  my ($trie,$str) = @_;
  my ($i,$q,@path);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$trie->{cw}) {
    push(@path,$q);
    $q = $trie->{goto}[$q]{substr($str,$i,$trie->{cw})};
  }
  push(@path,$q) if (defined($q));
  return \@path;
}

## $out = $trie->q2out($q)
sub q2out { return defined($_[1]) ? $_[0]{out}{$_[1]} : undef; }

## $out = $trie->s2out($str)
sub s2out { return $_[0]->q2out($_[0]->s2q($_[1])); }

## ($q,$len) = $trie->s2prefixQ($str)
## $q        = $trie->s2prefixQ($str)
sub s2prefixQ {
  my ($trie,$str) = @_;
  my ($q,$i,$qnext);
  for ($q=0,$i=0; $i < length($str); $q=$qnext,$i+=$trie->{cw}) {
    last if (!defined($qnext = $trie->{goto}[$q]{substr($str,$i,$trie->{cw})}));
  }
  return wantarray ? ($q,$i) : $q;
}

## $prefix = $trie->s2prefix($str)
sub s2prefix {
  my ($trie,$str) = @_;
  my ($q,$i) = $trie->s2prefixQ($str);
  return substr($str,0,$i);
}

##==============================================================================
## Methods: Export: Gfsm
##==============================================================================

## $labs = $trie->gfsmArcLabels()
## $labs = $trie->gfsmArcLabels($labs)
sub gfsmArcLabels {
  my ($trie,$labs) = @_;
  $labs = Gfsm::Alphabet->new if (!defined($labs));
  $labs->insert($trie->{epsilon},Gfsm::epsilon()) if (!defined($labs->find_key(Gfsm::epsilon())));
  $labs->insert($_) foreach (sort keys (%{$trie->{chars}}));
  return $labs;
}

## $qlabs = $trie->gfsmStateLabels()
## $qlabs = $trie->gfsmStateLabels($qlabs)
sub gfsmStateLabels {
  require Gfsm;
  my ($trie,$qlabs) = @_;
  $qlabs = Gfsm::Alphabet->new if (!defined($qlabs));
  foreach (0..($trie->{nq}-1)) {
    $qlabs->insert(("q${_}:<".$trie->q2s($_).">"
		    .(defined($trie->{out}{$_}) ? "=$trie->{out}{$_}" : '')),
		   $_);
  }
  return $qlabs;
}

## $gfsmTrie = $trie->gfsmTrie()
## $gfsmTrie = $trie->gfsmTrie($arcLabels)
## $gfsmTrie = $trie->gfsmTrie($arcLabels,$gfsmTrie)
sub gfsmTrie {
  my ($trie,$labs,$fsm) = @_;
  $labs = $trie->gfsmArcLabels() if (!defined($labs));
  $fsm = Gfsm::Automaton->newTrie if (!defined($fsm));
  $fsm->is_transducer(0);
  $fsm->is_weighted(0);
  $fsm->root(0);
  my ($q,$qah,$a,$qto);
  foreach $q (0..($trie->{nq}-1)) {
    if (defined($qah = $trie->{goto}[$q])) {
      while (($a,$qto)=each(%$qah)) {
	$fsm->add_arc($q,$qto, $labs->get_label($a),Gfsm::epsilon(), 0);
      }
    }
    if (exists($trie->{out}{$q})) {
      $fsm->is_final($q,1);
    }
  }
  $fsm->arcsort(Gfsm::ASMLower());
  return $fsm;
}

##==============================================================================
## Methods: Debug: View
##==============================================================================

## undef = $trie->viewps(%options)
sub viewps {
  my ($trie,%args) = @_;
  my $qlabs = $args{states} ? $args{states} : $trie->gfsmStateLabels;
  my $alabs = $args{labels} ? $args{labels} : $trie->gfsmArcLabels;
  my $fsm   = $args{fsm}    ? $args{fsm}    : $trie->gfsmTrie($alabs);
  $fsm->viewps(labels=>$alabs,states=>$qlabs,%args);
}

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
