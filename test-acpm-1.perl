#!/usr/bin/perl -w

use lib qw(.);
use Lingua::LTS;
use Lingua::LTS::Trie;
use Lingua::LTS::ACPM;
use Gfsm;

use Storable qw(store freeze thaw);

##--------------------------------------------------------------
## Test set: #1
sub test1 {
  our @special = ('<epsilon>', '<fail>', '#');
  our @letters = qw(a b c);
  our @phones = qw(A B P);
  our @rules = (
		{lhs=>[],        in=>[qw(a)],   rhs=>[],        out=>['A'] },  ##--   [a:A]
		#{lhs=>[qw(b a)], in=>[qw(b)],   rhs=>[qw(b)],   out=>['']  },  ##-- ba[b:]bb
		#{lhs=>[qw(b a)], in=>[qw(b b)], rhs=>[qw(a b)], out=>['B'] },  ##-- ba[bb:B]ab
		{lhs=>[qw(b a)], in=>[qw(b)],   rhs=>[],        out=>['P'] },  ##-- ba[b:P]
		{lhs=>[],        in=>[qw(b)],   rhs=>[],        out=>['B'] },  ##--   [b:B]
	       );
  $rules[$_]{id}=$_ foreach (0..$#rules);
  @rulestrs = map { rule2str($_) } @rules;
}

##--------------------------------------------------------------
## Test set: from LTS file
sub testlts {
  my $ltsfile = shift;
  our $lts = Lingua::LTS->new;
  $lts->load($ltsfile);
  $lts->expand_alphabet;
  @special = sort keys %{$lts->{specials}};
  @letters = sort ( keys(%{$lts->{letters}}), keys(%{$lts->{classes}}) );
  @phones  = sort keys %{$lts->{phones}};
  @rules   = @{$lts->{rules}};
  @rulestrs = map { rule2str($_) } @rules;
}

##-- expanded
sub testltsx {
  my $ltsfile = shift;
  our $lts = Lingua::LTS->new;
  $lts->load($ltsfile);
  $lts->expand_alphabet;
  $lts->expand_rules;
  @special = sort keys %{$lts->{specials}};
  @letters = sort ( keys(%{$lts->{letters}}), keys(%{$lts->{classes}}) );
  @phones  = sort keys %{$lts->{phones}};
  @rules   = @{$lts->{rulex}};
  $rules[$_]{id}=$_ foreach (0..$#rules);
}

##--------------------------------------------------------------
## Test sets: LTS files

sub testltstest { testltsx('test.lts'); }
sub testltstestx { testltsx('test.lts'); }

sub testltstest2 { testltsx('test2.lts'); }
sub testltstest2x { testltsx('test2.lts'); }

sub testims   { testlts('lts-ims-german.lts'); }
sub testimsx  { testltsx('lts-ims-german.lts'); }
sub testims2  { testlts('lts-ims-german-2.lts'); }
sub testims2x { testltsx('lts-ims-german-2.lts'); }

##--------------------------------------------------------------
## Debug: FSM Viewing

BEGIN { our %fstargs = (out2str=>\&out2str_ruleids); }

sub viewfsm {
  my $fsm = shift;
  $fsm->viewps(bg=>1,states=>$qlabs_p,@_);
}

sub viewtrie    { $trie->viewps(bg=>1,states=>$tqlabs_d,title=>'Trie',@_); }
sub viewtriefst { $trie->viewfst(bg=>1,states=>$tqlabs_p,%fstargs,title=>'Trie/FST',@_); }

sub viewacpm    { $acpm->viewps(bg=>1,states=>$qlabs_d,title=>'ACPM',@_); }
sub viewacpmfst { $acpm->viewfst(bg=>1,states=>$qlabs_p,%fstargs,title=>'ACPM/FST',@_); }

sub viewacpm0    { $acpm0->viewps(bg=>1,states=>$qlabs_d,title=>'ACPM_0',@_); }
sub viewacpm0fst { $acpm0->viewfst(bg=>1,states=>$qlabs_p,%fstargs,title=>'ACPM_0/FST',@_); }

##--------------------------------------------------------------
## Generation: Trie: Native
sub gentrie {
  our $trie = Lingua::LTS::Trie->new(cw=>1);

  my ($r,%rstrs,$rstr,$q);
  our $q2rule = {};
  foreach $r (@rules) {
    %rstrs = map { $_=>join('',@{$r->{$_}}) } (qw(lhs in out rhs));
    $rstr  = join('', @rstrs{qw(lhs in rhs)});
    #$q    = $trie->add($rstr, "$r->{id}:$rstrs{lhs}\[$rstrs{in}:$rstrs{out}\]$rstrs{rhs}");
    if (defined($q=$trie->s2q($rstr))) {
      $trie->{out}{$q}{$r->{id}} = undef;
    } else {
      $q = $trie->add($rstr,{$r->{id}=>undef});
    }
    $q2rule->{$q} = $r;
  }

  our $tqlabs_d = $trie->gfsmStateLabels(undef,out2str=>\&out2str_ruleids);
  our $tqlabs_p = $trie->gfsmStateLabels(undef,out2str=>undef);

  our $tolabs_d = $trie->gfsmOutputLabels(undef,out2str=>\&out2str_ruleids);
}
#test1;
#gentrie;

##--------------------------------------------------------------
## Utilities
sub out2str_ruleids {
  return (defined($_[0])
	  ? ('\\n= '
	     .join('\\n| ',
		   map { rule2str($rules[$_]) }
		   sort { $a<=>$b } keys(%{$_[0]})))
	  : '');
}


##--------------------------------------------------------------
## Generation: ACPM: native
sub joinout_str {
  return (defined($_[0])
	  ? (defined($_[1]) ? "$_[0] | $_[1]" : $_[0])
	  : (defined($_[1]) ? $_[1]         : '')
	 );
}
sub joinout_hash {
  if (defined($_[0])) {
    @{$_[0]}{keys %{$_[1]}} = undef if (defined($_[1]));
    return $_[0];
  }
  return {%{$_[1]}} if (defined($_[1]));
}
sub genacpm {
  gentrie;
  #our $acpm0  = Lingua::LTS::ACPM->newFromTrie($trie,joinout=>\&joinout_hash);
  our $acpm0  = Lingua::LTS::ACPM->new();
  $acpm0->fromTrie($trie,joinout=>\&joinout_hash);
  our $acpm   = $acpm0->clone->complete;

  our $qlabs_d = $acpm->gfsmStateLabels(undef,out2str=>\&out2str_ruleids);
  our $qlabs_p = $acpm->gfsmStateLabels(undef,out2str=>undef);
}

##--------------------------------------------------------------
## generate a gfsm transducer
sub lts2fst {
  #$lts->{implicit_bos}=$lts->{implicit_eos}=0;

  our $acpm = $lts->toACPM();
  our $qlabs_d = $acpm->gfsmStateLabels(undef,out2str=>\&out2str_ruleids);
  our $qlabs_p = $acpm->gfsmStateLabels(undef,out2str=>undef);

  our $iolabs = $lts->gfsmLabels();
  our $fst    = $lts->gfsmTransducer(ilabels=>$iolabs,olabels=>$iolabs,verbose=>1);

  our $acpm1       = $acpm->clone;
  $acpm1->{out}{$_} = $lts->{q2rulpos}[$_] foreach (0..$#{$lts->{q2rulpos}});
  our $qlabs_rp    = $acpm1->gfsmStateLabels(undef,out2str=>\&out2str_rulepos);

  viewfsm($fst,labels=>$iolabs,states=>$qlabs_p,title=>'LTS->FST');
  #$acpm1->viewps(bg=>1,states=>$qlabs_rp,title=>"ACPM_1");

  #our $acpm1c = $acpm1->clone->complete;
  #$acpm1c->viewps(bg=>1,states=>$qlabs_rp,title=>"ACPM_1 (complete)");
}
#testltstestx;
#testltstest2x;
#lts2fst;

##--------------------------------------------------------------
## Test: FST (2a)
sub lts2fst_2a {
  ##-- build left-, right-context ACPM
  print STDERR "gen(lacpm)...\n";
  our $lacpm = rules2acpm($lts,which=>[qw(lhs)]);
  print STDERR "gen(racpm)...\n";
  our $racpm = rules2acpm($lts,which=>[qw(in rhs)],reversed=>1);

  ##-- labels
  print STDERR "gen(lqlabs)...\n";
  our $lqlabs   = $lacpm->gfsmStateLabels(undef,out2str=>\&out2str_ruleids_short);
  print STDERR "gen(rqlabs)...\n";
  our $rqlabs  = $racpm->gfsmStateLabels(undef,out2str=>\&out2str_ruleids_short);

  ##-- complete
  print STDERR "complete(lacpm)...\n";
  our $lacpmc = $lacpm->clone->complete;
  print STDERR "complete(racpm)...\n";
  our $racpmc = $racpm->clone->complete;

  ##-- viewing
  $lacpm->viewps(bg=>1,title=>'LHS ACPM',out2str=>\&out2str_ruleids);
  $racpm->viewps(bg=>1,title=>'RHS ACPM (reversed)',out2str=>\&out2str_ruleids);
}
#testltstest2x;
#testims2x();
#lts2fst_2a;

##--------------------------------------------------------------
## Test: FST (2b)
sub lts2fst_2b {
  ##-- LHS: build left-context ACPM
  print STDERR "gen(lacpm)...\n";
  our $lacpm = rules2acpm($lts,which=>[qw(lhs)], packout=>1);
  our $lacpm0 = $lacpm->clone;
  print STDERR "complete(lacpm)...\n";
  $lacpm->complete;

  ##-- LHS: get i/o alphabet for lacpm
  our $lilabs  = $lacpm->gfsmInputLabels();
  our $nlilabs = $lilabs->size;
  our @lisyms  = grep { $_ ne '<eps>' && $_ ne '<fail>' } @{$lilabs->asArray};

  our $lolabs   = $lacpm->gfsmOutputLabels(undef, epsilon=>'<eps>');
  our $nlolabs  = $lolabs->size;
  $lolabs->insert($_) foreach (@lisyms);

  our $lolabs_d = $lacpm->gfsmOutputLabels(undef, epsilon=>'<eps>',out2str=>\&out2str_packed);
  $lolabs_d->insert($_) foreach (@lisyms);

  our $lqlabs   = $lacpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- LHS: build & adjust lacpm fst
  our $lfst = $lacpm->gfsmTransducer(ilabels=>$lilabs,olabels=>$lolabs);
  our $qmax = $lfst->n_states();
  our $ai = Gfsm::ArcIter->new;
  foreach $q (0..($lacpm->{nq}-1)) {
    %rdeltaq = qw();
    for ($ai->open($lfst,$q); $ai->ok; $ai->next) {
      if (!exists($rdeltaq{$qto=$ai->target})) {
	$rdeltaq{$qto} = $qmax;
	$lfst->add_arc($qmax,$qto, 0,$ai->upper, 0);
	$lqlabs->insert("q$qmax", $qmax);
	$qmax++;
      }
    }
    for ($ai->reset; $ai->ok; $ai->next) {
      $ai->upper($lolabs->find_label($lilabs->find_key($ai->lower)));
      $ai->target($rdeltaq{$ai->target});
    }
  }

  ##-------------------------
  ## RHS
  print STDERR "gen(racpm)...\n";
  our $racpm = rules2acpm($lts,which=>[qw(in rhs)], reversed=>1, packout=>1);
  our $racpm0 = $racpm->clone;
  print STDERR "complete(racpm)...\n";
  $racpm->complete;

  ##-- get i/o alphabet for lacpm
  our $rilabs   = $racpm->gfsmInputLabels(Storable::dclone($lolabs),   epsilon=>'<eps>');
  our $rilabs_d = Storable::dclone($rilabs);
  foreach (1..($nlolabs-1)) {
    $key = $rilabs->find_key($_);
    $rilabs_d->remove_key($key);
    $rilabs_d->insert(out2str_packed($key), $_);
  }

  our $rolabs   = $racpm->gfsmOutputLabels(undef, epsilon=>'<eps>');
  our $nrolabs  = $rolabs->size;
  $rolabs->insert($_) foreach (@{$lolabs->asArray}[1..($nlolabs-1)]);

  our $rolabs_d = $racpm->gfsmOutputLabels(undef, epsilon=>'<eps>', out2str=>\&out2str_packed);
  $rolabs_d->insert($_) foreach (@{$lolabs_d->asArray}[1..($nlolabs-1)]);

  our $rqlabs   = $racpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- RHS: build & adjust racpm fst
  our $rfst = $racpm->gfsmTransducer(ilabels=>$rilabs,olabels=>$rolabs);
  $qmax = $rfst->n_states();
  $ai = Gfsm::ArcIter->new;
  foreach $q (0..($racpm->{nq}-1)) {
    ##-- state for having read ruleset id out(LACPM), pending letter in(LACPM)
    $r = $qmax++;
    $rqlabs->insert("q${r}~q$q", $r);

    ##-- re-route old letter arcs from (q--a:b-->qto) to (r--a:b-->qto)
    for ($ai->open($rfst,$q); $ai->ok; $ai->reset) {
      $rfst->add_arc($r,$ai->target, $ai->lower,$ai->upper, 0);
      $ai->remove;
    }

    ##-- add rule-arcs (q--R:R-->r) for each possible rule in out(LACPM)
    foreach $i (1..($nlolabs-1)) {
      $key = $lolabs->find_key($i);
      $lolab = $rilabs->find_label($key);
      $hilab = $rolabs->find_label($key);
      $rfst->add_arc($q,$r, $lolab,$hilab, 0);
    }
  }

  ##-- LHS ° RHS
  print STDERR "compose(lfst,rfst->reverse)...\n";
  our $cfst = $lfst->compose($rfst->reverse);

  ##-- LHS: view
  #$lacpm->viewps(bg=>1,title=>'LACPM',out2str=>\&out2str_packed);
  #viewfsm($lfst,lower=>$lilabs,upper=>$lolabs_d,states=>$lqlabs,bg=>1);

  ##-- RHS: view
  #$racpm0->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #$racpm->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #viewfsm($rfst,lower=>$rilabs,upper=>$rolabs_d,states=>$rqlabs,bg=>1);

  ##-- composition: view
  viewfsm($cfst,lower=>$lilabs,upper=>$rolabs_d,bg=>1);

  #$w='abba';
  #$resl = $lfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
  #$resr = $rfst->lookup([reverse(@{$rilabs->asHash}{@{$lolabs->asArray}[@{$resl->paths->[0]{hi}}]})])->connect->reverse->rmepsilon;
  $resc = $cfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
}
testltstest2x;
#testims2x();
lts2fst_2b;

##-- utility: out2str_packed()
sub out2str_packed {
  if (defined($_[0])) {
    return ('\\n{' . join(',', unpack('S*',$_[0])) . '}');
  }
  return '\\n-undef-';
}

##-- utility: rules2acpm($lts,%args)
## + %args:
##    reversed=>$bool,
##    which=>\@rule_keys,
##    packout=>$bool,     ##-- whether to pack output function
##    complete=>$bool,
sub rules2acpm {
  my ($lts,%args) = @_;
  our $trie = Lingua::LTS::Trie->new();
  @{$trie->{chars}}{keys(%{$lts->{letters}}),keys(%{$lts->{specials}})} = undef;
  my ($q,$r,@rsyms,$rstr);
  foreach $r (@{$lts->{rulex}}) {
    @rsyms = map { @$_ } @$r{@{$args{which}}};
    $rstr  = join('', $args{reversed} ? reverse(@rsyms) : @rsyms);
    if (defined($q=$trie->s2q($rstr))) {
      $trie->{out}{$q}{$r->{id}}=undef;
    } else {
      $trie->add($rstr,{$r->{id}=>undef});
    }
  }
  my $acpm = Lingua::LTS::ACPM->newFromTrie($trie,
					    joinout=>\&Lingua::LTS::_acpm_joinout
					   );

  ##-- pack output function
  if ($args{packout}) {
    foreach $q (0..($acpm->{nq}-1)) {
      $acpm->{out}{$q} = pack('S*',
			      (defined($acpm->{out}{$q})
			       ? (map { $_+1 } sort { $a <=> $b } keys(%{$acpm->{out}{$q}}))
			       : qw()));
    }
  }

  $acpm->complete() if ($args{complete});
  return $acpm;
}


##-- utility: out2str_ruleids_short
sub out2str_ruleids_short {
  return (defined($_[0])
	  ? ('='.join('|',sort { $a<=>$b } keys(%{$_[0]})))
	  : '');
}

##--------------------------------------------------------------
## Test: lookup (FST)

## undef = fstlkp($w)
sub fstlkp {
  my $w = shift;
  $w = '' if (!defined($w));
  @phones_lts = $lts->apply_word_indexed($w);

  @wlabs  = grep { defined($_) } @{$iolabs->asHash}{(($lts->{implicit_bos} ? '#' : qw()),
						     split(//,$w),
						     ($lts->{implicit_eos} ? '#' : qw()))};
  $result = $fst->lookup(\@wlabs);
  $paths  = $result->paths(Gfsm::LSUpper);

  our (@phones_fst);
  print
    ("w=\"$w\"\n",
     "\tLTS: ", join(' ', '(', @phones_lts, ')'), "\n",
     (map {
       ("\tFST: ", join(' ', '(', (@phones_fst=@{$iolabs->asArray}[@{$_->{hi}}]), ')'), "\n")
     } @$paths),
    );
}


##--------------------------------------------------------------
## Utility: out2str_rulepos
sub out2str_rulepos {
  if (defined($_[0])) {
    my @rulpos   = keys %{$_[0]};
    ##
    ##-- get symbolic index %rp2match
    my ($rp,$lenL,$lenLI,$lenLIR,$nread,$rulid, $matchid);
    my %rp2match = qw();
    foreach $rp (@rulpos) {
      ($lenL,$lenLI,$lenLIR,$nread,$rulid) = unpack('S4L', $rp);
      $matchid = pack('S4',$lenL,$lenLI,$lenLIR,$nread);
      $rp2match{$rp} = {rulid=>$rulid,
			nread=>$nread,
			lenL=>$lenL,
			lenLI=>$lenLI,
			lenLIR=>$lenLIR,
			matchBegin=>($lenL-$nread),
			matchEnd=>($lenLI-$nread),
			matchBuf=>($lenLIR-$nread),
			#matchid=>$matchid,
		       };
    }
    ##
    ##-- generate output string
    my ($match,$rul,@rsyms);
    return ('\\n= '
	    .join('\\n| ',
		  map {
		    $match = $rp2match{$_};
		    $rul   = $rules[$match->{rulid}];
		    @rsyms = (@{$rul->{lhs}},@{$rul->{in}},@{$rul->{rhs}});
		    $rsyms[$_] = '?' foreach (scalar(@rsyms)..($match->{nread}-1));
		    @rsyms[$match->{nread}-1] .= "_";
		    splice(@rsyms, scalar(@{$rul->{lhs}}+@{$rul->{in}}), 0, ':', @{$rul->{out}}, ']');
		    splice(@rsyms, scalar(@{$rul->{lhs}}),               0, '[');
		    (''
		     .' ~ '
		     ."{$match->{lenL}..$match->{lenLI}($match->{lenLIR})}"
		     ." \@ $match->{nread}"
		     ." ~ {$match->{matchBegin}..$match->{matchEnd}($match->{matchBuf})}"
		     ." -> $rul->{id}"
		     .' : '.join('', @rsyms)
		     )
		  } sort {
		    #($b->{lenLIR}   <=> $a->{lenLIR}
		    # || $b->{lenLI} <=> $a->{lenLI}
		    # || $b->{lenL}  <=> $a->{lenL}
		    # || $a->{rulid} <=> $b->{rulid})
		    $b cmp $a
		  }
		  keys(%rp2match)
		  #values(%rp2match)
		 ));
  }
  return '';
}

sub out2str_rulepos_0 {
  if (defined($_[0])) {
    my @rulpos   = keys %{$_[0]};
    my ($rp,$lenL,$lenLI,$lenLIR,$nread,$rulid, $matchBegin,$matchEnd,$matchid);
    my %match2rp = qw();
    foreach $rp (@rulpos) {
      ($lenL,$lenLI,$lenLIR,$nread,$rulid) = unpack('S4L', $rp);
      $matchBegin = $lenL-$nread;
      $matchEnd   = $lenLI-$nread;
      $matchBuf   = $lenLIR-$nread;
      $matchid = pack('sss',$matchBegin,$matchEnd,$matchBuf);
      if (!exists($match2rp{$matchid}) || $match2rp{$matchid}{rulid} > $rulid) {
	$match2rp{$matchid} = {rulid=>$rulid,
			       nread=>(@{$rules[$rulid]{lhs}}-$matchBegin),
			       lenL=>$lenL,
			       lenLI=>$lenLI,
			       lenLIR=>$lenLIR,
			       matchBegin=>$matchBegin,
			       matchEnd=>$matchEnd,
			       matchBuf=>$matchBuf,
			      };
      }
    }
    my ($m1id,$m2id,$m1,$m2);
    ##-- eliminate overlap
    foreach $m1id (keys(%match2rp)) {
      next if (!defined($m1=$match2rp{$m1id}));
      foreach $m2id (keys(%match2rp)) {
	$m2 = $match2rp{$m2id};
	next if ($m1 eq $m2 || $m1->{matchBuf} > 0 || $m2->{matchBuf} > 0);
	if ($m1->{rulid} < $m2->{rulid}
	    && $m1->{matchBegin} <= $m2->{matchBegin}
	    && $m1->{matchEnd}   >= $m2->{matchBegin})
	  {
	    delete($match2rp{$m2id});
	  }
      }
    }
    ##-- output string
    my ($match,$rul,$i0,@rsyms);
    return ('\\n= '
	    .join('\\n| ',
		  map {
		    $match = $match2rp{$_};
		    $rul   = $rules[$match->{rulid}];
		    @rsyms = (@{$rul->{lhs}},@{$rul->{in}},@{$rul->{rhs}});
		    $rsyms[$_] = '?' foreach (scalar(@rsyms)..($match->{nread}-1));
		    @rsyms[$match->{nread}-1] .= "_";
		    splice(@rsyms, scalar(@{$rul->{lhs}}+@{$rul->{in}}), 0, ':', @{$rul->{out}}, ']');
		    splice(@rsyms, scalar(@{$rul->{lhs}}),               0, '[');
		    (''
		     ." ~ "
		     ."{$match->{matchBegin}..$match->{matchEnd}($match->{matchBuf})} -> "
		     ."$rul->{id} : "
		     .join('', @rsyms)
		     )
		  } sort {
		    @al=unpack('s3',$a);
		    @bl=unpack('s3',$b);
		    ($al[0] <=> $bl[0]
		     || $al[1] <=> $bl[1]
		     || $al[2] <=> $bl[2])
		  } keys(%match2rp)));
  }
  return '';
}


##--------------------------------------------------------------
## DEBUG: rule to string
sub rule2str {
  my $rul = shift;
  my %ruls = map { $_=>join('', @{$rul->{$_}}) } qw(lhs in out rhs);
  return ((defined($rul->{id}) ? $rul->{id} : '??')
	  .":$ruls{lhs}\[$ruls{in}:$ruls{out}\]$ruls{rhs}"
	 );
}

##--------------------------------------------------------------
## Test: lookup
sub lkptest {
  my $str = shift;
  our @matches = $acpm->matches($str);
  our @dmatches = (
		  map {
		    (($_ > 0 ? substr($str,$_-1,1) : '<bos>')
		     ." ~ "
		     .(defined($matches[$_])
		       ? join(' | ', map { rule2str($rules[$_]) } sort {$a<=>$b} keys(%{$matches[$_]}))
		       : '-undef-'))
		  } (0..$#matches)
		  );
  our @mrules = qw();
  our ($rul);
  foreach $i (0..@matches) {
    next if (!defined($matches[$i]));
    foreach $ruli (keys %{$matches[$i]}) {
      $rul   = $rules[$ruli];
      $j     = $i - @{$rul->{rhs}} - @{$rul->{in}} + 1;
      $mruli = $mrules[$j];
      $mrules[$j] = $ruli if (!defined($mruli) || $mruli > $ruli);
    }
  }
  our @dmrules = (
		  map {
		    (($_>0 ? substr($str,$_-1,1) : '<bos>')
		     ." ~ "
		     .(defined($mrules[$_]) ? rule2str($rules[$mrules[$_]]) : '-undef-'))
		  } (0..$#mrules)
		 );
  our @out = qw();
  for ($i=0; $i <= $#mrules; $i++) {
    next if (!defined($mrules[$i]));
    $rul = $rules[$mrules[$i]];
    if (defined($rul)) {
      push(@out, @{$rul->{out}});
      $i += $#{$rul->{in}};
    }
  }
  $out = join(' ', @out);
  print "lts($str) = $out\n";
}
#test1;
#genacpm;
##lkptest('ababbab');
#lkptest('babbab');
##--
#testims2x;
#genacpm;
##lkptest('#schied#');
#lkptest('#unterschied#');


#

##--- main: dummy
foreach $i (0..5) {
  print "--dummy[$i]--\n";
}
