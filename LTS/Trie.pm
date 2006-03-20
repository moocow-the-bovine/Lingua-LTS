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
  return bless Storable::dclone($trie), ref($trie)||$trie;
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

## $qid = $trie->addArray(\@syms,$val)
sub addArray {
  my ($trie,$ary,$val) = @_;
  my ($i,$q,$a,$qnext);
  for ($q=0,$i=0; $i <= $#$ary; $q=$qnext, $i++) {
    $a = $ary->[$i];
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

## undef = $trie->remove(\@syms)
##  + hack: just removes output for @syms
sub removeArray {
  my ($trie,$ary) = @_;
  my $q = $trie->a2q($ary);
  delete($trie->{out}{$q}) if (defined($q));
}

##==============================================================================
## Methods: Output packing
##==============================================================================

## $trie = $trie->packout(%args)
##  + suitable for tries whose output function is a hash pseudo-set with numeric keys
##  + %args:
##     packas => $pack_template_char,  ##-- either 'L' or 'S', default='L'
##     packadd=> $int,                 ##-- value to add to ids when packing (default=0)
sub packout {
  my ($trie,%args) = @_;
  my $packas  = ($args{packas}  ? $args{packas} : 'L').'*';
  my $packadd =  $args{packadd} ? $args{packadd} : 0;
  my ($q);
  foreach $q (0..($trie->{nq}-1)) {
    $trie->{out}{$q} = pack($packas,
			    (defined($trie->{out}{$q})
			     ? (map { $_+$packadd }
				sort { $a <=> $b }
				keys(%{$trie->{out}{$q}}))
			     : qw()));
  }
  return $trie;
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

## $q = $trie->a2q(\@ary)
sub a2q {
  my ($trie,$ary) = @_;
  my ($q,$i);
  for ($q=0,$i=0; defined($q) && $i <= $#$ary; $i++) {
    $q = $trie->{goto}[$q]{$ary->[$i]};
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

## \@syms = $trie->q2a($q);
sub q2a {
  my ($trie,$q) = @_;
  my @syms = qw();
  my ($qa,$a);
  while (defined($q) && $q > 0 && defined($qa=$trie->{rgoto}[$q])) {
    ($q,$a) = split(/ /, $qa, 2);
    push(@syms, $a);
  }
  return [reverse(@syms)];
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

## \@states = $trie->a2path(\@syms)
sub a2path {
  my ($trie,$ary) = @_;
  my ($i,$q,@path);
  for ($q=0,$i=0; defined($q) && $i <= $#$ary; $i++) {
    push(@path,$q);
    $q = $trie->{goto}[$q]{$ary->[$i]};
  }
  push(@path,$q) if (defined($q));
  return \@path;
}


## $out = $trie->q2out($q)
sub q2out { return defined($_[1]) ? $_[0]{out}{$_[1]} : undef; }

## $out = $trie->s2out($str)
sub s2out { return $_[0]->q2out($_[0]->s2q($_[1])); }

## $out = $trie->a2out(\@syms)
sub a2out { return $_[0]->q2out($_[0]->a2q($_[1])); }

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

## ($q,$len) = $trie->a2prefixQ(\@syms)
## $q        = $trie->a2prefixQ(\@syms)
sub a2prefixQ {
  my ($trie,$ary) = @_;
  my ($q,$i,$qnext);
  for ($q=0,$i=0; $i <= $#$ary; $q=$qnext,$i++) {
    last if (!defined($qnext = $trie->{goto}[$q]{$ary->[$i]}));
  }
  return wantarray ? ($q,$i) : $q;
}

## $prefix = $trie->s2prefix($str)
sub s2prefix {
  my ($trie,$str) = @_;
  my ($q,$i) = $trie->s2prefixQ($str);
  return substr($str,0,$i);
}

## $prefix = $trie->a2prefix(\@syms)
sub a2prefix {
  my ($trie,$ary) = @_;
  my ($q,$i) = $trie->a2prefixQ($ary);
  return @$ary[0..($i-1)];
}


##==============================================================================
## Methods: Export: Gfsm
##==============================================================================

## $ilabs = $trie->gfsmInputLabels()
## $ilabs = $trie->gfsmInputLabels($labs)
## $ilabs = $trie->gfsmInputLabels($labs,%args)
##  + input alphabet
##  + %args:
##     epsilon => $eps_str, ##-- default: $trie->{epsilon}
sub gfsmInputLabels {
  my ($trie,$labs,%args) = @_;
  $labs = Gfsm::Alphabet->new if (!defined($labs));
  $labs->insert((defined($args{epsilon}) ? $args{epsilon} : $trie->{epsilon}),
		Gfsm::epsilon())
    if (!defined($labs->find_key(Gfsm::epsilon())));
  $labs->insert($_) foreach (sort keys (%{$trie->{chars}}));
  return $labs;
}

## $olabs = $trie->gfsmOutputLabels()
## $olabs = $trie->gfsmOutputLabels($labs,%args)
##  + output alphabet
##  + %args:
##     out2str => \&sub,    ##-- get key for $out: default: stringify
##     epsilon => $eps_str, ##-- default: $trie->{epsilon}
sub gfsmOutputLabels {
  my ($trie,$labs,%args) = @_;
  my $out2str = exists($args{out2str}) ? $args{out2str} : undef;
  $labs = Gfsm::Alphabet->new if (!defined($labs));
  $labs->insert((defined($args{epsilon}) ? $args{epsilon} : $trie->{epsilon}),
		Gfsm::epsilon())
    if (!defined($labs->find_key(Gfsm::epsilon())));
  my ($out);
  foreach $out (values(%{$trie->{out}})) {
    $labs->insert((defined($out2str) ? $out2str->($out)
		   : (defined($out) ? $out : '')));
  }
  return $labs;
}

## $qlabs = $trie->gfsmStateLabels()
## $qlabs = $trie->gfsmStateLabels($qlabs)
## $qlabs = $trie->gfsmStateLabels($qlabs,%args)
##  + state alphabet
##  + %args:
##     out2str => \&sub ##-- called as $sub->($out_or_undef) : default: stringify
sub gfsmStateLabels {
  require Gfsm;
  my ($trie,$qlabs,%args) = @_;
  $qlabs = Gfsm::Alphabet->new if (!defined($qlabs));
  my $out2str = exists($args{out2str}) ? $args{out2str} : \&out2str_default;
  foreach (0..($trie->{nq}-1)) {
    $qlabs->insert(("q${_}:<".$trie->q2s($_).">"
		    .(defined($out2str) ? $out2str->($trie->{out}{$_}) : '')),
		   $_);
  }
  return $qlabs;
}

## $outstr = out2str_default($out_or_undef)
sub out2str_default { return defined($_[0]) ? "=$_[0]" : ''; }

## $gfsmDFA = $trie->gfsmAutomaton(%args)
##  + %args:
##     fsm=>$fsm,            ##-- output automaton
##     ilabels =>$inLabels,  ##-- default: $trie->gfsmInputLabels()
##     dosort=>$bool,        ##-- sort automaton? (default=yes)
sub gfsmAutomaton {
  my ($trie,%args) = @_;
  my $ilabs = defined($args{ilabels})  ? $args{ilabels} : $trie->gfsmInputLabels(undef,%args);
  my $fsm   = defined($args{fsm})      ? $args{fsm}     : Gfsm::Automaton->newTrie();
  $fsm->is_transducer(0);
  $fsm->is_weighted(0);
  $fsm->root(0);
  my ($q,$qah,$a,$alab,$qto);
  foreach $q (0..($trie->{nq}-1)) {
    if (defined($qah = $trie->{goto}[$q])) {
      while (($a,$qto)=each(%$qah)) {
	$alab = $ilabs->get_label($a);
	$fsm->add_arc($q,$qto, $alab,$alab, 0);
      }
    }
    if (exists($trie->{out}{$q})) {
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
##     out2str =>\&sub,      ##-- as for StateLabels(), OutputLabels()
##     dosort=>$bool,        ##-- sort automaton? (default=yes)
sub gfsmTransducer {
  my ($trie,%args) = @_;
  my $olabs = defined($args{olabels}) ? $args{olabels} : $trie->gfsmOutputLabels(undef,%args);
  my $fsm   = $trie->gfsmAutomaton(%args);
  my $out2str = $args{out2str};
  $fsm->is_transducer(1);
  my ($q,$out);
  my $ai = Gfsm::ArcIter->new;
  foreach $q (0..($trie->{nq}-1)) {
    for ($ai->open($fsm,$q); $ai->ok; $ai->next) {
      next if (!defined($out=$trie->{out}{$ai->target}));
      $ai->upper($olabs->find_label(defined($out2str) ? $out2str->($out) : $out));
    }

  }
  return $fsm;
}

##==============================================================================
## Methods: Debug: View
##==============================================================================

## undef = $trie->viewps(%options)
sub viewps {
  my ($trie,%args) = @_;
  my $qlabs = exists($args{states}) ? exists($args{states}) : $trie->gfsmStateLabels(undef,%args);
  my $ilabs = exists($args{labels}) ? exists($args{labels}) : $trie->gfsmInputLabels(undef,%args);
  my $fsm   = $args{fsm}    ? $args{fsm}    : $trie->gfsmAutomaton(ilabels=>$ilabs);
  $fsm->viewps(labels=>$ilabs,states=>$qlabs,%args);
}

## undef = $trie->drawDot($dotfile,%options)
sub drawDot {
  my ($trie,$dotfile,%args) = @_;
  my $qlabs = exists($args{states}) ? exists($args{states}) : $trie->gfsmStateLabels(undef,%args);
  my $ilabs = exists($args{labels}) ? exists($args{labels}) : $trie->gfsmInputLabels(undef,%args);
  my $fsm   = $args{fsm}    ? $args{fsm}    : $trie->gfsmAutomaton(ilabels=>$ilabs);
  $fsm->draw_dot($dotfile,labels=>$ilabs,states=>$qlabs,%args);
}


## undef = $trie->viewfst(%options)
sub viewfst {
  my ($trie,%args) = @_;
  my $qlabs = exists($args{states})  ? $args{states}  : $trie->gfsmStateLabels(undef,%args);
  my $ilabs = exists($args{ilabels}) ? $args{ilabels} : $trie->gfsmInputLabels(undef,%args);
  my $olabs = exists($args{olabels}) ? $args{olabels} : $trie->gfsmOutputLabels(undef,%args);
  my $fsm   = $args{fsm} ? $args{fsm} : $trie->gfsmTransducer(ilabels=>$ilabs,olabels=>$olabs,%args);
  $fsm->viewps(lower=>$ilabs,upper=>$olabs,states=>$qlabs,%args);
}

## undef = $trie->fstDot($dotfile,%options)
sub fstDot {
  my ($trie,$dotfile,%args) = @_;
  my $qlabs = exists($args{states})  ? $args{states}  : $trie->gfsmStateLabels(undef,%args);
  my $ilabs = exists($args{ilabels}) ? $args{ilabels} : $trie->gfsmInputLabels(undef,%args);
  my $olabs = exists($args{olabels}) ? $args{olabels} : $trie->gfsmOutputLabels(undef,%args);
  my $fsm   = $args{fsm} ? $args{fsm} : $trie->gfsmTransducer(ilabels=>$ilabs,olabels=>$olabs,%args);
  $fsm->draw_dot($dotfile,lower=>$ilabs,upper=>$olabs,states=>$qlabs,%args);
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
