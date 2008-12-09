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
  print STDERR "testlts($ltsfile)... ";
  our $lts = Lingua::LTS->new;
  $lts->load($ltsfile);
  $lts->expand_alphabet;
  @special = sort keys %{$lts->{specials}};
  @letters = sort ( keys(%{$lts->{letters}}), keys(%{$lts->{classes}}) );
  @phones  = sort keys %{$lts->{phones}};
  @rules   = @{$lts->{rules}};
  @rulestrs = map { rule2str($_) } @rules;
  print STDERR "done.\n";
}

##-- expanded
sub testltsx {
  my $ltsfile = shift;
  print STDERR "testltsx($ltsfile)... ";
  our $lts = Lingua::LTS->new;
  $lts->load($ltsfile);
  $lts->expand_alphabet;
  $lts->expand_rules;
  @special = sort keys %{$lts->{specials}};
  @letters = sort ( keys(%{$lts->{letters}}), keys(%{$lts->{classes}}) );
  @phones  = sort keys %{$lts->{phones}};
  @rules   = @{$lts->{rulex}};
  $rules[$_]{id}=$_ foreach (0..$#rules);
  print STDERR "done.\n";
}

##-- expanded
sub testltsxb {
  my $ltsfile = shift;
  print STDERR "testltsxb($ltsfile)... ";
  our $lts = Lingua::LTS->new;
  $lts->load($ltsfile);
  $lts->expand_alphabet();
  $lts->sanitize_rules();
  $lts->expand_rules();
  @special = sort keys %{$lts->{specials}};
  @letters = sort ( keys(%{$lts->{letters}}), keys(%{$lts->{classes}}) );
  @phones  = sort keys %{$lts->{phones}};
  @rules   = @{$lts->{rulex}};
  #$rules[$_]{id}=$_ foreach (0..$#rules);
  print STDERR "done.\n";
}

##--------------------------------------------------------------
## Test sets: LTS files

sub testltstest { testltsx('test.lts'); }
sub testltstestx { testltsx('test.lts'); }
sub testltstestxb { testltsxb('test.lts'); }

sub testltstest2 { testltsx('test2.lts'); }
sub testltstest2x { testltsx('test2.lts'); }
sub testltstest2xb { testltsxb('test2.lts'); }

sub testims   { testlts('lts-ims-german.lts'); }
sub testimsx  { testltsx('lts-ims-german.lts'); }
sub testims2  { testlts('lts-ims-german-2.lts'); }
sub testims2x { testltsx('lts-ims-german-2.lts'); }
sub testims2xb { testltsxb('lts-ims-german-2.lts'); }

##--------------------------------------------------------------
## Debug: verbose messages
BEGIN { our $progname = 'test-acpm.perl'; }
sub vmsg {
  print STDERR "$progname: ", @_;
}
sub vmsg0 {
  print STDERR @_;
}

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

sub out2str_lts_ruleids {
  return (defined($_[0])
	  ? ('\\n= '
	     .join('\\n| ',
		   map { rule2str($lts->{rules}[$_]) }
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
  our $lfst = $lacpm->gfsmTransducer(ilabels=>$lilabs,olabels=>$lolabs,dosort=>0);
  our $qmax = $lfst->n_states();
  our $ai = Gfsm::ArcIter->new;
  foreach $q (0..($lacpm->{nq}-1)) {
    ##-- replace arcs ($q --a:OutL($q)--> $qto) with ($q --a:OutL($q)--> $r=newState() --eps:a--> $qto)
    for ($ai->open($lfst,$q); $ai->ok; $ai->next) {
      $r = $qmax++;
      $lqlabs->insert("q${r}~r(q${q}-".$lilabs->find_key($ai->lower)."->q".$ai->target.")", $r);
      $lfst->add_arc($r,$ai->target, 0,$lolabs->find_label($lilabs->find_key($ai->lower)), 0);
      $ai->upper($lolabs->find_label($lacpm->{out}{$q}));
      $ai->target($r);
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
  our $rilabs   = $lolabs;
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

  our $colabs_d = Gfsm::Alphabet->new();
  $colabs_d->insert('<eps>',0);
  $colabs_d->insert('<none>',1);
  $colabs_d->insert(rule2str($lts->{rules}[$_]), $_+2) foreach (0..$#{$lts->{rules}});

  our $rqlabs   = $racpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- RHS: save LHS output hashes (indexed by lolabs)
  our $lolab2outh = [{}];
  foreach $loseti (1..($nlolabs-1)) {
    $lolab2outh->[$loseti] = { map { ($_=>undef) } unpack('S*', $lolabs->find_key($loseti))  };
  }

  ##-- RHS: build & adjust racpm fst
  our $rfst = $racpm->gfsmTransducer(ilabels=>$rilabs,olabels=>$rolabs,dosort=>0);
  $qmax = $rfst->n_states();
  $ai = Gfsm::ArcIter->new;
  foreach $q (0..($racpm->{nq}-1)) {
    ##-- $rdeltaq{$qto} = $r
    ##  + s.t. $r is a new intermediate state for arcs ($q --a:a--> $qto),
    ##  + translated to ($q --a:eps--> $r --OutL:OutBest(OutL,$qto)--> $qto)
    %rdeltaq = qw();
    for ($ai->open($rfst,$q); $ai->ok; $ai->next) {
      if (!exists($rdeltaq{$qto=$ai->target})) {
	$rdeltaq{$qto} = $r = $qmax;

	##-- re-route arcs ($q --a:a--> $qto) ==> ($q --a:eps--> $r)
	$ai->target($r);
	$ai->upper(0);

	$rqlabs->insert("q$qmax~r(q${q}->q${qto})", $qmax);
	$qmax++;

	##-- get best output for each left-context set outL \in out(Q_L), outR($qto)
	##   + add arcs: ($r --OutL:OutBest(OutL,$qto)--> $qto)
	$routp = $racpm->{out}{$qto};
	foreach $loseti (1..($nlolabs-1)) {
	  $loutp = $lolabs->find_key($loseti);
	  $louth = $lolab2outh->[$loseti];

	  $hilab = undef;
	  for ($routpi=0; $routpi < length($routp)/2; $routpi++) {
	    $routid = unpack('S',substr($routp,$routpi*2,2));
	    if (exists($louth->{$routid})) {
	      $hilab = $routid+1;
	      last;
	    }
	  }

	  ##-- add arc: $r --OutL:OutBest(OutL,$qto)--> $qto
	  $lolab = $rilabs->find_label($loutp);
	  $rfst->add_arc($r,$qto, $lolab, (defined($hilab) ? $hilab : 1), 0);
	}
      }
    }
  }

  ##-------------------------
  ## Ouput filter

  ##-- output filter: labels
  our $folabs = Gfsm::Alphabet->new;
  $folabs->insert('<eps>',0);
  $folabs->insert($_) foreach (sort(keys(%{$lts->{phones}})));

  ##-- output filter: fst
  our $filter = Gfsm::Automaton->new;
  $filter->is_transducer(1);
  $filter->is_weighted(0);
  $filter->root(0);
  $filter->is_final(0,1);
  $filter->add_arc(0,0, 1,0, 0);
  our @consume = (0,0);
  our ($rulout);
  our $fqmax = 1;
  foreach $rul (@{$lts->{rules}}) {
    $rulid = $rul->{id};
    $inlen = @{$rul->{in}};

    ##-- get consume path: ( $consume[$inlen] -- (AnyRule:<eps>)^${inlen} --> 0 )
    if (!defined($consume[$inlen])) {
      ##-- add consume states from $#consume..$inlen
      foreach $clen (grep { !defined($consume[$_]) } (1..$inlen)) {
	$qfrom = $consume[$clen] = $fqmax++;
	$qto   = $consume[$clen-1];

	##-- add an arc from consume($n) to consume($n-1) for each output rule
	foreach $colab (1..($#{$lts->{rules}}+2)) {
	  $filter->add_arc($qfrom, $qto, $colab,0, 0);
	}
      }
    }

    ##-- build output path (0 -- $rulid+2 : OUT($rul) --> $consume[$inlen])
    $rulout = $rul->{out};
    $filter->add_arc(0,        ($#$rulout <= 0 ? $consume[$inlen]                 : ($fqmax++)),
		     $rulid+2, ($#$rulout >= 0 ? $folabs->get_label($rulout->[0]) : 0),
		     0);
    foreach $i (1..$#$rulout) {
      $filter->add_arc($fqmax-1, ($i==$#$rulout ? $consume[$inlen] : ($fqmax++)),
		       0,        $folabs->get_label($rulout->[$i]),
		       0);
    }
  }
  $filter->arcsort(Gfsm::ASMLower);

  ##-- LHS ° RHS ° filter
  $lfst->arcsort(Gfsm::ASMUpper);
  $rfst->arcsort(Gfsm::ASMLower);
  print STDERR "compose(lfst,rfst->reverse)...\n";
  our $cfst0 = $lfst->compose($rfst->reverse);
  $cfst0->_connect;
  $cfst0->renumber_states;
  our $cfst = $cfst0->clone;
  $cfst->arcsort(Gfsm::ASMUpper);
  $cfst->_compose($filter);
  $cfst->_connect;
  $cfst->renumber_states;

  ##-- Filter: view
  #viewfsm($filter,title=>'Filter',lower=>$colabs_d,upper=>$folabs,bg=>1);

  ##-- LHS: view
  #$lacpm->viewps(bg=>1,title=>'LACPM',out2str=>\&out2str_packed);
  #viewfsm($lfst,title=>'LFST',lower=>$lilabs,upper=>$lolabs_d,states=>$lqlabs,bg=>1);

  ##-- RHS: view
  #$racpm0->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #$racpm->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #viewfsm($rfst,title=>'RFST',lower=>$rilabs_d,upper=>$colabs_d,states=>$rqlabs,bg=>1);

  ##-- composition: view
  viewfsm($cfst,title=>'CFST',lower=>$lilabs,upper=>$colabs_d,bg=>1);

  #$w='abba';
  #$resl = $lfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
  #$resr = $rfst->lookup([reverse(@{$rilabs->asHash}{@{$lolabs->asArray}[@{$resl->paths->[0]{hi}}]})])->connect->reverse->rmepsilon;
  #$resc = $cfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
}
#testltstest2x;
#lts2fst_2b;

##--------------------------------------------------------------
## Test: FST (2b): just get basic data
sub lts2fst_basic {
  ##-- LHS: build left-context ACPM
  print STDERR "gen(lacpm)...\n";
  our $lacpm = rules2acpm($lts,which=>[qw(lhs)], packout=>1);

  ##-- LHS: get i/o alphabet for lacpm
  our $lilabs  = $lacpm->gfsmInputLabels();
  our $nlilabs = $lilabs->size;
  our @lisyms  = grep { $_ ne '<eps>' && $_ ne '<fail>' } @{$lilabs->asArray};

  our $lolabs   = $lacpm->gfsmOutputLabels(undef, epsilon=>'<eps>');
  our $nlolabs  = $lolabs->size;
  $lolabs->insert($_) foreach (@lisyms);
  our $lolabs_a  = $lolabs->asArray;

  our $rilabs   = $lolabs;
  our $rilabs_d = Storable::dclone($rilabs);
  foreach (1..($nlolabs-1)) {
    $key = $rilabs->find_key($_);
    $rilabs_d->remove_key($key);
    $rilabs_d->insert(out2str_packed($key), $_);
  }

  ##-- RHS
  print STDERR "gen(racpm)...\n";
  our $racpm = rules2acpm($lts,which=>[qw(in rhs)], reversed=>1, packout=>1);

  our $rolabs   = $racpm->gfsmOutputLabels(undef, epsilon=>'<eps>');
  our $nrolabs  = $rolabs->size;
  $rolabs->insert($_) foreach (@{$lolabs->asArray}[1..($nlolabs-1)]);

  our $colabs_d = Gfsm::Alphabet->new();
  $colabs_d->insert('<eps>',0);
  $colabs_d->insert('<none>',1);
  $colabs_d->insert(rule2str($lts->{rules}[$_]), $_+2) foreach (0..$#{$lts->{rules}});

  print "ok.\n";
}
##testims2x;
#testims2; $lts->{rulex}=$lts->{rules}; ##-- hack
#lts2fst_basic;

##--------------------------------------------------------------
## Test: FST (2c)
sub lts2fst_2c {
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
  our $lolabs_a  = $lolabs->asArray;

  our $lolabs_d = $lacpm->gfsmOutputLabels(undef, epsilon=>'<eps>',out2str=>\&out2str_packed);
  $lolabs_d->insert($_) foreach (@lisyms);

  our $lqlabs   = $lacpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- LHS: build & adjust lacpm fst
  our $lfst = $lacpm->gfsmTransducer(ilabels=>$lilabs,olabels=>$lolabs,dosort=>0);
  our $qmax = $lfst->n_states();
  our $ai = Gfsm::ArcIter->new;
  foreach $q (0..($lacpm->{nq}-1)) {
    ##-- replace arcs ($q --a:OutL($q)--> $qto) with ($q --a:OutL($q)--> $r=newState() --eps:a--> $qto)
    for ($ai->open($lfst,$q); $ai->ok; $ai->next) {
      $r = $qmax++;
      $lqlabs->insert("q${r}~r(q${q}-".$lilabs->find_key($ai->lower)."->q".$ai->target.")", $r);
      $lfst->add_arc($r,$ai->target, 0,$lolabs->find_label($lilabs->find_key($ai->lower)), 0);
      $ai->upper($lolabs->find_label($lacpm->{out}{$q}));
      $ai->target($r);
    }
  }

  ##-------------------------
  ## RHS
  print STDERR "gen(racpm)...\n";
  our $racpm = rules2acpm($lts,which=>[qw(in rhs)], reversed=>1, packout=>1);
  our $racpm0 = $racpm->clone;
  print STDERR "complete(racpm)...\n";
  $racpm->complete;

  ##-- get i/o alphabet for racpm
  our $rilabs   = $lolabs;
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

  our $colabs_d = Gfsm::Alphabet->new();
  $colabs_d->insert('<eps>',0);
  $colabs_d->insert('<none>',1);
  $colabs_d->insert(rule2str($lts->{rules}[$_]), $_+2) foreach (0..$#{$lts->{rules}});

  our $rqlabs   = $racpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- IN.RHS: map ($packed_rulid == $rulid+1) => \@lolabs_containing_rulid
  our $outid2lolabs = [[]];
  our ($loseti,$prulid);
  foreach $loseti (1..($nlolabs-1)) {
    foreach $prulid (unpack('S*', $lolabs_a->[$loseti])) {
      push(@{$outid2lolabs->[$prulid]}, $loseti);
    }
  }


  ##-- RHS: build & adjust racpm fst
  our $rfst = $racpm->gfsmTransducer(ilabels=>$rilabs,olabels=>$rolabs,dosort=>0);
  $qmax = $rfst->n_states();
  $ai = Gfsm::ArcIter->new;
  foreach $q (0..($racpm->{nq}-1)) {
    ##-- $rdeltaq{$qto} = $r
    ##  + s.t. $r is a new intermediate state for arcs ($q --a:a--> $qto),
    ##  + translated to ($q --a:eps--> $r --OutL:OutBest(OutL,$qto)--> $qto)
    %rdeltaq = qw();
    for ($ai->open($rfst,$q); $ai->ok; $ai->next) {
      if (!exists($rdeltaq{$qto=$ai->target})) {
	$rdeltaq{$qto} = $r = $qmax;

	##-- re-route arcs ($q --a:a--> $qto) ==> ($q --a:eps--> $r)
	$ai->target($r);
	$ai->upper(0);

	$rqlabs->insert("q$qmax~r(q${q}->q${qto})", $qmax);
	$qmax++;

	##-- get best output for each left-context set outL \in out(Q_L), outR($qto)
	##   + add arcs: ($r --OutL:OutBest(OutL,$qto)--> $qto)
	%usedlolabs = qw();
	if ($racpm->{out}{$qto}) {
	  foreach $routid (unpack('S*',$racpm->{out}{$qto})) {
	    $lolabids = $outid2lolabs->[$routid];
	    foreach $lolabid (defined($lolabids) ? @$lolabids : qw()) {
	      if (!exists($usedlolabs{$lolabid})) {
		$rfst->add_arc($r,$qto, $lolabid, $routid+1, 0);
		$usedlolabs{$lolabid}=1;
	      }
	    }
	  }
	} else {
	  $rfst->add_arc($r,$qto, 1,1, 0);
	}
      }
    }
  }

  ##-------------------------
  ## Ouput filter

  ##-- output filter: labels
  our $folabs = Gfsm::Alphabet->new;
  $folabs->insert('<eps>',0);
  $folabs->insert($_) foreach (sort(keys(%{$lts->{phones}})));

  ##-- output filter: fst
  our $filter = Gfsm::Automaton->new;
  $filter->is_transducer(1);
  $filter->is_weighted(0);
  $filter->root(0);
  $filter->is_final(0,1);
  $filter->add_arc(0,0, 1,0, 0);
  our @consume = (0,0);
  our ($rulout);
  our $fqmax = 1;
  foreach $rul (@{$lts->{rules}}) {
    $rulid = $rul->{id};
    $inlen = @{$rul->{in}};

    ##-- get consume path: ( $consume[$inlen] -- (AnyRule:<eps>)^${inlen} --> 0 )
    if (!defined($consume[$inlen])) {
      ##-- add consume states from $#consume..$inlen
      foreach $clen (grep { !defined($consume[$_]) } (1..$inlen)) {
	$qfrom = $consume[$clen] = $fqmax++;
	$qto   = $consume[$clen-1];

	##-- add an arc from consume($n) to consume($n-1) for each output rule
	foreach $colab (1..($#{$lts->{rules}}+2)) {
	  $filter->add_arc($qfrom, $qto, $colab,0, 0);
	}
      }
    }

    ##-- build output path (0 -- $rulid+2 : OUT($rul) --> $consume[$inlen])
    $rulout = $rul->{out};
    $filter->add_arc(0,        ($#$rulout <= 0 ? $consume[$inlen]                 : ($fqmax++)),
		     $rulid+2, ($#$rulout >= 0 ? $folabs->get_label($rulout->[0]) : 0),
		     0);
    foreach $i (1..$#$rulout) {
      $filter->add_arc($fqmax-1, ($i==$#$rulout ? $consume[$inlen] : ($fqmax++)),
		       0,        $folabs->get_label($rulout->[$i]),
		       0);
    }
  }
  $filter->arcsort(Gfsm::ASMLower);

  ##-- LHS ° RHS ° filter
  $lfst->arcsort(Gfsm::ASMUpper);
  $rfst->arcsort(Gfsm::ASMLower);
  print STDERR "compose(lfst,rfst->reverse)...\n";
  our $cfst0 = $lfst->compose($rfst->reverse);
  $cfst0->_connect;
  $cfst0->renumber_states;
  our $cfst = $cfst0->clone;
  $cfst->arcsort(Gfsm::ASMUpper);
  $cfst->_compose($filter);
  $cfst->_connect;
  $cfst->renumber_states;

  ##-- Filter: view
  #viewfsm($filter,title=>'Filter',lower=>$colabs_d,upper=>$folabs,bg=>1);

  ##-- LHS: view
  #$lacpm->viewps(bg=>1,title=>'LACPM',out2str=>\&out2str_packed);
  #viewfsm($lfst,title=>'LFST',lower=>$lilabs,upper=>$lolabs_d,states=>$lqlabs,bg=>1);

  ##-- RHS: view
  #$racpm0->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #$racpm->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  viewfsm($rfst,title=>'RFST',lower=>$rilabs_d,upper=>$colabs_d,states=>$rqlabs,bg=>1);

  ##-- composition: view
  viewfsm($cfst,title=>'CFST',lower=>$lilabs,upper=>$colabs_d,bg=>1);

  #$w='abba';
  #$resl = $lfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
  #$resr = $rfst->lookup([reverse(@{$rilabs->asHash}{@{$lolabs->asArray}[@{$resl->paths->[0]{hi}}]})])->connect->reverse->rmepsilon;
  #$resc = $cfst->lookup([@{$lilabs->asHash}{split(//,$w)}])->connect;
}
#testltstest2x;
##testims2x;
#testims2; $lts->{rulex}=$lts->{rules}; ##--hack
#lts2fst_2c;

##--------------------------------------------------------------
## Test: FST (2d)
sub lts2fst_2d {
  ##-- LHS: build left-context ACPM
  print STDERR "gen(lacpm)...\n";
  our $lacpm = rules2acpm($lts,which=>[qw(lhs)], packout=>1);
  our $lacpm0 = $lacpm->clone;
  print STDERR "complete(lacpm)...\n";
  $lacpm->complete;

  ##-- LHS: get i/o alphabet for lacpm
  our $lilabs  = $lacpm->gfsmInputLabels();
  $lilabs->remove_label($lilabs->find_label($lacpm->{failstr}));
  our $nlilabs = $lilabs->size;

  our $lolabs   = Gfsm::Alphabet->new();
  $lolabs->merge($lilabs);
  our $norullab = $lolabs->insert('<norule>');
  our $ruloffset = $lolabs->size;
  $lolabs->insert($_) foreach (@{$lts->{rules}});
  our $lolabs_a = $lolabs->asArray;

  our $lolabs_d = Storable::dclone($lolabs);
  $lolabs_d->insert(rule2str($lolabs_a->[$_]), $_) foreach ($ruloffset..$#$lolabs_a);

  our $lqlabs   = $lacpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- LHS: build & adjust lacpm fst
  print STDERR "gen(lfst)...\n";
  our $lfst = $lacpm->gfsmTransducer(ilabels=>$lilabs,olabels=>undef,dosort=>0);
  our $qmax = $lfst->n_states();
  our $ai = Gfsm::ArcIter->new;
  foreach $q (0..($lacpm->{nq}-1)) {
    print STDERR "." if ($q % 10 ==0);

    ##-- replace arcs ($q --a:OutL($q)--> $qto)
    ##   with { ($q --a:${rullab}--> $r=newState() --eps:a--> $qto) }
    ##   for each $rullab \in OutL($q)
    @newarcs = qw();
    @outq    = unpack('S*', $lacpm->{out}{$q});
    for ($ai->open($lfst,$q); $ai->ok; $ai->reset()) {
      $r = $qmax++;
      ($lo,$qto) = ($ai->lower,$ai->target);

      $lqlabs->insert("q${r}~r(q${q}-".$lilabs->find_key($ai->lower)."->q".$ai->target.")", $r); ##--debug

      ##-- add arc ($r --eps:a--> $qto)
      $lfst->add_arc($r,$qto, 0,$lo, 0);

      ##-- collect arcs ($q --a:${rullab}--> $r)
      push(@newarcs, map { [$r,$lo,$_+$ruloffset-1] } @outq);

      $ai->remove();
    }
    ##-- add arcs ($q --a:${rullab}--> $r)
    $lfst->add_arc($q,$_->[0], $_->[1],$_->[2], 0) foreach (@newarcs);
  }

  ##-------------------------
  ## RHS
  print STDERR "gen(racpm)...\n";
  our $racpm = rules2acpm($lts,which=>[qw(in rhs)], reversed=>1, packout=>1);
  our $racpm0 = $racpm->clone;
  print STDERR "complete(racpm)...\n";
  $racpm->complete;

  ##-- get i/o alphabet for racpm
  our $rilabs   = $lolabs;
  our $rilabs_d = $lolabs_d;

  our $rolabs   = $lolabs;
  our $rolabs_d = $lolabs_d;

  our $rqlabs   = $racpm->gfsmStateLabels(undef, out2str=>undef);

  ##-- RHS: build & adjust racpm fst
  print STDERR "gen(rfst)...\n";
  our $rfst = $racpm->gfsmTransducer(ilabels=>$rilabs,olabels=>undef,dosort=>0);
  $qmax = $rfst->n_states();
  $ai = Gfsm::ArcIter->new;
  foreach $q (0..($racpm->{nq}-1)) {
    ##-- $rdeltaq{$qto} = $r
    ##  + s.t. $r is a new intermediate state for arcs ($q --a:a--> $qto),
    ##  + translated to ($q --a:eps--> $r --${rullab}:${rullab}--> $qto)
    ##    s.t. ${rullab} \in OutR($qto)
    %rdeltaq = qw();
    for ($ai->open($rfst,$q); $ai->ok; $ai->next) {
      if (!exists($rdeltaq{$qto=$ai->target})) {
	$rdeltaq{$qto} = $r = $qmax;

	##-- re-route arcs ($q --a:a--> $qto) ==> ($q --a:eps--> $r)
	$ai->target($r);
	$ai->upper(0);

	$rqlabs->insert("q$qmax~r(q${q}->q${qto})", $qmax);
	$qmax++;

	##-- add arcs: ($r --${rullab}:${rullab}--> $qto)
	foreach $rulid (unpack('S*',$racpm->{out}{$qto})) {
	  $rfst->add_arc($r,$qto, $rulid+$ruloffset-1,$rulid+$ruloffset-1, 0);
	}
      }
    }
  }

  ##-- compose
  print STDERR "reverse(rfst)...\n";
  our $rrfst = $rfst->reverse;

  print STDERR "compose(lfst,rfst)...\n";
  our $cfst = $lfst->compose($rfst);
  $cfst->_connect;
  $cfst->renumber_states;

  ##-- LHS: view
  #$lacpm->viewps(bg=>1,title=>'LACPM',out2str=>\&out2str_packed);
  #viewfsm($lfst,title=>'LFST',lower=>$lilabs,upper=>$lolabs_d,states=>$lqlabs,bg=>1);

  ##-- RHS: view
  #$racpm->viewps(bg=>1,title=>'RACPM',out2str=>\&out2str_packed);
  #viewfsm($rfst,title=>'RFST',lower=>$rilabs_d,upper=>$rolabs_d,states=>$rqlabs,bg=>1);

  ##-- composition: view
  viewfsm($cfst,title=>'LFST°reverse(RFST)',lower=>$lilabs,upper=>$rolabs_d,bg=>1);
}
##testltstest2x;
#testims2xb;
#lts2fst_2d;
#exit(0);

##--------------------------------------------------------------
## Test: FST (2e)
sub lts2fst_2e {
  my %args = @_;
  my $debug = exists($args{debug}) ? $args{debug} : 0;

  ##----------------------------
  ## LHS

  ##-- LHS: build left-context ACPM
  vmsg("gen(ltrie)... ");
  our $ltrie = subtrie($lts, which=>[qw(lhs)], rules=>$lts->{rules}, %args);
  vmsg0("done.\n");

  vmsg("gen(lacpm)... ");
  our $lacpm  = Lingua::LTS::ACPM->newFromTrie($ltrie,joinout=>\&Lingua::LTS::_acpm_joinout);
  our $lacpm0 = $lacpm->clone() if ($debug);  ##-- DEBUG: $lacpm0 : initial
  vmsg0("done.\n");

  vmsg("complete(lacpm)... ");
  $lacpm->complete();
  our $lacpm_c = $lacpm->clone() if ($debug); ##-- DEBUG: $lacpm_c : completed
  vmsg0("done.\n");

  vmsg("expand(lacpm)... ");
  #$lacpm = expandACPM($lacpm,$lts->{classes},joinout=>\&Lingua::LTS::_acpm_joinout,%args);
  $lacpm->expand($lts->{classes}, packas=>'S', joinout=>\&Lingua::LTS::_acpm_joinout);
  our $lacpm_x = $lacpm->clone() if ($debug); ##-- DEBUG: $lacpm_x : completed, expanded
  vmsg0("done.\n");

  vmsg("packout(lacpm)... ");
  packout($lacpm, packadd=>0);
  our $lacpm_p = $lacpm->clone() if ($debug); ##-- DEBUG: $lacpm_p : completed, expanded, packed
  vmsg0("done.\n");

  ##-- debug
  if ($debug) {
    vmsg("debug(lacpm)... ");
    our $xlacpm = rulex2acpm($lts,which=>[qw(lhs)], expand=>0, packout=>1, debug=>$debug);
    vmsg0("done.\n");
  }

  ##-- LHS: alphabets
  vmsg("alphabets(lacpm)... ");
  my $ilabs = Gfsm::Alphabet->new;
  $ilabs->insert('<eps>', 0);

  my $sharedlabs = Gfsm::Alphabet->new;
  $sharedlabs->insert('<eps>', 0);

  if ($debug) {
    ##--DEBUG
    our $sharedlabs_d = Gfsm::Alphabet->new;
    $sharedlabs_d->insert('<eps>', 0);

    our $lqlabs = $lacpm->gfsmStateLabels(undef,out2str=>undef);
  }
  vmsg0("done.\n");

  ##-- LHS: construct fst
  vmsg("fst(lacpm)... ");
  my $lfst = Gfsm::Automaton->new();
  $lfst->sort_mode(Gfsm::ASMNone);
  $lfst->is_transducer(1);
  $lfst->is_weighted(0);
  $lfst->root(0);
  my ($gotoq,$c,$qoutp,$lo,$hi);
  foreach $q (0..($lacpm->{nq}-1)) {
    $gotoq = $lacpm->{goto}[$q];
    ##-- add arcs
    while (($c,$qto)=each(%$gotoq)) {
      $lo = $ilabs->get_label($c);
      $hi = $sharedlabs->get_label(pack('S', $lo).$lacpm->{out}{$q});
      $lfst->add_arc($q,$qto, $lo,$hi, 0);

      if ($debug) {
	##--DEBUG
	$sharedlabs_d->insert(("$c~".out2str_packed_lts_long($lacpm->{out}{$q})), $hi);
      }
    }
    ##-- check for state finality
    $lfst->is_final($q,1) if (exists($lacpm->{out}{$q}));
  }
  vmsg0("done.\n");

  ##-- LHS: norule
  vmsg('lhs: norule');
  our $norulid = scalar(@{$lts->{rules}});
  $sharedlabs->insert(pack('SS', $_, $norulid)) foreach (1..($ilabs->size-1));
  if ($debug) {
    $sharedlabs_d->insert(($sharedlabs_d->find_key($_)."~<norule>"),
			  $sharedlabs->find_label(pack('SS',$_,$norulid)))
      foreach (1..($ilabs->size-1));
  }

  ##-- LHS: keepers: arcs (q --a:a--> q) for q \in Q_{LHS}, c \in %keep
  my $nshared_packed = $sharedlabs->size;
  foreach $c (keys(%{$lts->{keep}})) {
    $lo = $ilabs->get_label($c);
    $hi = $sharedlabs->get_label(pack('SS', $lo, $norulid));
    $sharedlabs_d->insert("${c}~<norule>", $hi) if ($debug);

    foreach $q (0..($lfst->n_states-1)) {
      $lfst->add_arc($q,$q, $lo,$hi, 0);
    }
  }
  vmsg0("done.\n");


  ##----------------------------
  ## IN+RHS

  ##-- IN+RHS: build in+right-context ACPM
  vmsg("gen(rtrie)... ");
  our $rtrie = subtrie($lts, which=>[qw(in rhs)], reversed=>1, rules=>$lts->{rules}, %args);
  vmsg0("done.\n");

  vmsg("gen(racpm)... ");
  our $racpm  = Lingua::LTS::ACPM->newFromTrie($rtrie,joinout=>\&Lingua::LTS::_acpm_joinout);
  our $racpm0 = $racpm->clone() if ($debug);  ##-- DEBUG: $racpm0 : initial
  vmsg0("done.\n");

  vmsg("complete(racpm)... ");
  $racpm->complete();
  our $racpm_c = $racpm->clone() if ($debug); ##-- DEBUG: $racpm_c : completed
  vmsg0("done.\n");

  vmsg("expand(racpm)... ");
  #$racpm = expandACPM($racpm,$lts->{classes},joinout=>\&Lingua::LTS::_acpm_joinout,%args);
  $racpm->expand($lts->{classes}, packas=>'S', joinout=>\&Lingua::LTS::_acpm_joinout);
  our $racpm_x = $racpm->clone() if ($debug); ##-- DEBUG: $racpm_x : completed, expanded
  vmsg0("done.\n");

  vmsg("packout(racpm)... ");
  packout($racpm, packadd=>0);
  our $racpm_p = $racpm->clone() if ($debug); ##-- DEBUG: $racpm_p : completed, expanded, packed
  vmsg0("done.\n");

  if ($debug) {
    ##-- DEBUG
    vmsg("debug(racpm)... ");
    our $xracpm =
      rulex2acpm($lts,which=>[qw(in rhs)], reversed=>1, expand=>0, packout=>1, debug=>$debug);
    foreach $q (0..($xracpm->{nq}-1)) {
      $xracpm->{out}{$q} = pack('S*', map { $_-1 } unpack('S*',$xracpm->{out}{$q}));
    }
    vmsg0("done.\n");
  }

  ##-- IN+RHS: alphabets
  vmsg("alphabets(racpm)... ");
  my $rullabs = Gfsm::Alphabet->new;
  $rullabs->insert('<eps>', 0);
  $rullabs->insert($lts->{rules}[$_], $_+1) foreach (0..$#{$lts->{rules}});
  $rullabs->insert({lhs=>[],rhs=>[],in=>[],out=>[],id=>$norulid}, $norulid+1);
  my $nrullabs_ids = $rullabs->size;
  ##-- <keep=STR>
  $rullabs->insert("<keep=$_>") foreach (sort(keys(%{$lts->{keep}})));

  if ($debug) {
    our $rullabs_d = Gfsm::Alphabet->new;
    $rullabs_d->insert('<eps>', 0);

    ##-- rules
    $rullabs_d->insert(rule2str($lts->{rules}[$_]), $_+1)
      foreach (0..$#{$lts->{rules}});

    ##--  <norule>
    $rullabs_d->insert('<norule>', $norulid+1);    ##-- <norule>

    ##-- <keep=STR>
    $rullabs_d->insert("<keep=$_>") foreach (sort(keys(%{$lts->{keep}})));

    our $rqlabs = $racpm->gfsmStateLabels(undef,out2str=>undef);
  }
  vmsg0("done.\n");

  ##-- IN+RHS: indexing
  vmsg("indexing shared alphabet... ");
  my $cr2shared = {}; ##-- maps ($ilab.' '.($rulid+0)) => \@sharedlabs_for_char_containing_rulid
  my $ilabs_a      = $ilabs->asArray;
  my $ilabs_h      = $ilabs->asHash;
  my $sharedlabs_a = $sharedlabs->asArray;
  my $sharedlabs_h = $sharedlabs->asHash;
  my ($ilab,@rulids,$sharedlab);
  foreach $sharedlab (1..($nshared_packed-1)) {
    ($ilab,@rulids) = unpack('S*', $sharedlabs_a->[$sharedlab]);
    foreach $rulid (@rulids) {
      push(@{$cr2shared->{$ilab.' '.$rulid}}, $sharedlab);
    }
  }
  vmsg0("done.\n");

  ##-- IN+RHS: fst construction
  vmsg("fst(racpm)... ");
  my $rfst = Gfsm::Automaton->new();
  $rfst->sort_mode(Gfsm::ASMNone);
  $rfst->is_transducer(1);
  $rfst->is_weighted(0);
  $rfst->root(0);
  my %warnedabout = qw();
  my ($sharedlab_matches,%used_shared_labs);
  foreach $q (0..($racpm->{nq}-1)) {
    $gotoq = $racpm->{goto}[$q];

    ##-- add arcs
    while (($c,$qto)=each(%$gotoq)) {
      $lo = $ilabs->get_label($c);

      if ($racpm->{out}{$qto}) {
	##-- get best output for each left-context set outL \in out(Q_L), outR($qto)
	##   + add arcs: ($q --(char,OutL):OutBest(OutL,$qto)--> $qto)
	%used_shared_labs = qw();
	foreach $rulid (unpack('S*', $racpm->{out}{$qto})) {
	  $sharedlab_matches = $cr2shared->{"$lo $rulid"};
	  foreach $sharedlab (
			      grep {!exists($used_shared_labs{$_})}
			      (defined($sharedlab_matches) ? @$sharedlab_matches : qw())
			     )
	    {
	      $rfst->add_arc($q,$qto, $sharedlab,$rulid+1, 0);
	      $used_shared_labs{$sharedlab}=undef;
	    }
	}
      } else {
	##-- no output defined for state: add a '<norule>' transition (and warn)
	if (!exists($warnedabout{$qto})) {
	  $lts->vmsg0('error', "\n");
	  $lts->vmsg('error', "no output defined for RACPM state q$qto -- using <norule>!\n");
	  $warnedabout{$qto} = undef;
	}

	$rfst->add_arc($q,$qto, $sharedlabs->get_label(pack('SS',$lo,$norulid)),$norulid+1, 0);
      }
    }
    ##-- check for state finality
    $rfst->is_final($q,1) if (exists($racpm->{out}{$q}));
  }

  ##-- RHS: keepers: arcs (q --a:a--> q) for q \in Q_{RHS}, c \in %keep
  foreach $c (keys(%{$lts->{keep}})) {
    $lo = $sharedlabs->get_label(pack('SS', $ilabs->find_label($c), $norulid));
    $hi = $rullabs->find_label("<keep=$c>");

    foreach $q (0..($rfst->n_states-1)) {
      $rfst->add_arc($q,$q, $lo,$hi, 0);
    }
  }

  vmsg0("done.\n");

  ##-------------------------
  ## Ouput filter

  ##-- output filter: labels
  vmsg("Output filter: labels...");

  my $folabs = Gfsm::Alphabet->new;
  $folabs->insert('<eps>',0);
  $folabs->insert($_) foreach (sort(keys(%{$lts->{phones}})));

  ##-- output filter: fst
  my $filter = Gfsm::Automaton->new;
  $filter->is_transducer(1);
  $filter->is_weighted(0);
  $filter->root(0);
  $filter->is_final(0,1);
  $filter->add_arc(0,0, $norulid+1,0, 0); ##-- <norule> (?)
  my @consume = (0,0);
  my ($rul,$inlen,$rulout,$qfrom,$clen,$rullab,$i);
  $qmax = 1;
  foreach $rul (@{$lts->{rules}}) {
    $rulid = $rul->{id};
    $inlen = @{$rul->{in}};

    ##-- get consume path: ( $consume[$inlen] -- (AnyRule:<eps>)^${inlen} --> 0 )
    if (!defined($consume[$inlen])) {
      ##-- add consume states from $#consume..$inlen
      foreach $clen (grep { !defined($consume[$_]) } (1..$inlen)) {
	$qfrom = $consume[$clen] = $qmax++;
	$qto   = $consume[$clen-1];

	##-- add an arc from consume($n) to consume($n-1) for each output rule
	foreach $rullab (1..($nrullabs_ids-1)) {
	  $filter->add_arc($qfrom, $qto, $rullab,0, 0);
	}
      }
    }

    ##-- build output path (0 -- $rulid+2 : OUT($rul) --> $consume[$inlen])
    $rulout = $rul->{out};
    $filter->add_arc(0,        ($#$rulout <= 0 ? $consume[$inlen]                 : ($qmax++)),
		     $rulid+1, ($#$rulout >= 0 ? $folabs->get_label($rulout->[0]) : 0),
		     0);
    foreach $i (1..$#$rulout) {
      $filter->add_arc($qmax-1, ($i==$#$rulout ? $consume[$inlen] : ($qmax++)),
		       0,       $folabs->get_label($rulout->[$i]),
		       0);
    }
  }

  ##-- Filter: keepers: arcs (q --a:a--> q) for q \in @consume, a \in %keep
  foreach $c (keys(%{$lts->{keep}})) {
    $lo = $rullabs->find_label("<keep=$c>");
    $hi = $folabs->get_label($c);
    foreach $q (@consume[1..$#consume]) {
      $filter->add_arc($q,$q, $lo,$hi, 0);
    }
  }

  vmsg0("done.\n");

  ##-------------------------
  ## Compose: LHS ° RHS
  vmsg("reverse(RHS)...");
  my $rrfst = $rfst->reverse;
  vmsg0("done.\n");

  vmsg("arcsort()...");
  $lfst->arcsort(Gfsm::ASMUpper);
  $rrfst->arcsort(Gfsm::ASMLower);
  $filter->arcsort(Gfsm::ASMUpper);
  vmsg0("done.\n");

  vmsg("cfst = compose(lfst,rfst)...");
  my $cfst = $lfst->compose($rrfst);
  vmsg0("done.\n");

  vmsg("cfst: connect, renumber, sort...");
  ##-- TODO
  #$cfst->_connect;
  #$cfst->renumber_states;
  ##--/TODO
  $cfst->arcsort(Gfsm::ASMUpper);
  vmsg0("done.\n");

  vmsg("fcfst = compose(cfst,filter)...");
  my $fcfst = $cfst->compose($filter);
  vmsg0("done.\n");

  vmsg("fcfst: connect, renumber, sort...");
  ##-- TODO
  #$fcfst->_connect;
  #$fcfst->renumber_states;
  ##-- /TODO
  $fcfst->arcsort(Gfsm::ASMLower);
  vmsg0("done.\n");

  ##----------------------------
  ## DEBUG
  if ($debug) {
    ##-- LHS: view
    #$ltrie0->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'LTrie (0)');
    #$ltrie->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'LTrie (+det)');
    #$lacpm0->viewps(out2str=>\&out2str_packed_lts_long,bg=>1,title=>'LACPM (+det,+fail)');
    #$lacpm->viewps(out2str=>\&out2str_packed_lts_long,bg=>1,title=>'LACPM (+det,-fail)')
    #viewfsm($lfst,lower=>$ilabs,upper=>$sharedlabs_d,states=>$lqlabs,bg=>1,title=>'LACPM FST');
    ##-- RHS: view
    #$rtrie0->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'RTrie (0)');
    #$rtrie->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'RTrie (+det)');
    #$racpm0->viewps(out2str=>\&out2str_packed_lts_long,bg=>1,title=>'RACPM (+det,+fail)');
    #$racpm->viewps(out2str=>\&out2str_packed_lts_long,bg=>1,title=>'RACPM (+det,-fail)');
    #viewfsm($rfst,lower=>$sharedlabs_d,upper=>$rullabs_d,states=>$rqlabs,bg=>1,title=>'RACPM FST');

    ##--ARGH
    # DB<390> x map { out2str_packed_lts_debug($_) } @{$racpm0->{out}}{@{$racpm0->s2path($wr='#netnu#')}}
    # 0  '{  }'
    # 1  '{ 87:[#:] }'
    # 2  '{ 39:[n:n] }'
    # 3  '{ 5:#C*[en:e:n]# | 6:[en:@n]# | 24:[e:e:]C | 25:[e:@] }'
    # 4  '{ 47:[t:t] }'
    # 5  '{ 86:[z:ts] }'
    # 6  '{ 64:[u:u:] }'
    # 7  '{ 87:[#:] }'

    ##-- lookup
    our $w='abba';
    our $resl = $lfst->lookup([grep {defined($_)} @{$ilabs->asHash}{split(//,$w)}])->connect;
    our $resr = $rfst->lookup([grep {defined($_)} reverse(@{$resl->paths->[0]{hi}})])->connect->reverse;
    #--
    our $resc  = $cfst->lookup([grep {defined($_)} @{$ilabs->asHash}{split(//,$w)}])->connect;
    our $rescf = $fcfst->lookup([grep {defined($_)} @{$ilabs->asHash}{split(//,$w)}])->connect;
    ##--
    our $phonlabs = $fcfst->lookup([grep {defined($_)} @{$ilabs->asHash}{split(//,$w)}])->connect->paths->[0]{hi};
    our $phones   = join(' ', @{$folabs->asArray}[@$phonlabs]);
  }
}
#testltstest2xb();
#lts2fst_2e(debug=>1);
##--
#testims2xb;
#lts2fst_2e(debug=>1);
#exit(0);

##--------------------------------------------------------------
## Test: 2f
sub lts2fst_2f {
  my %args = @_;
  our $labs = Gfsm::Alphabet->new();
  our $fst  = $lts->gfsmTransducer(ilabels=>$labs,olabels=>$labs,%args);

  ##-- DEBUG
  $lts->{apply_verbose}=1;
  fstlkp('ab~ba', $fst,lower=>$labs,upper=>$labs);
  #fstlkp('unterschied', $fst,lower=>$labs,upper=>$labs);

  ##-- SAVE
  my $saveas = $args{saveas} ? $args{saveas} : 'test2f';
  $fst->save("$saveas.gfst");
  $labs->save("$saveas.lab");
}
testltstest2xb();
lts2fst_2f();
##--
#testims2xb;
#lts2fst_2f();
exit(0);

##--------------------------------------------------------------
## Test: flex
sub testFlex {
  my $file = shift;
  $file = 'testFlex.flex' if (!defined($file));

  $lts->toFlex($file,
	       onlhs=>sub {
		 return ('printf("LHS: text=\'%s\', lhsid=%d, lc=('.join(' ', @$_)
			 .')\\n", yytext, lhsid);');
	       },
	       ontry=>sub {
		 return ('printf("TRY: text=\'%s\', lhsid=%d, rul=('.rule2str($_)
			 .')\\n", yytext, lhsid);')
	       },
	       onreject=>sub {
		 return ('printf("REJECT: text=\'%s\', lhsid=%d, rul=('.rule2str($_)
			 .') XXX\\n", yytext, lhsid);');
	       },
	       onmatch=>sub {
		 return ('printf("MATCH: text=\'%s\', lhsid=%d, rul=('.rule2str($_)
			 .') ***\\n", yytext, lhsid);');
	       },
	      );
}
#testims2xb;
##testltstest2xb;
#testFlex('testims2.flex');
#exit(0);


##--------------------------------------------------------------
## Debug: depth
sub q2depth {
  my $trie = shift;
  my @fifo = qw(0);
  my ($q,$qto,$qdepth);
  my @q2depth = qw(0);
  while (defined($q=shift(@fifo))) {
    $qdepth = $q2depth[$q];
    foreach $qto (values(%{$trie->{goto}[$q]})) {
      $q2depth[$qto] = $qdepth+1;
      push(@fifo,$qto);
    }
  }
  return \@q2depth;
}

##--------------------------------------------------------------
## Test: LTS::fst

sub gentfst {
  my $basename = shift;
  #our ($fst,$ilabs,$olabs) = $lts->gfsmTransducer(verbose=>1);
  our ($lfst,$rfst,$filter,$ilabs,$olabs) = $lts->gfsmTransducer(verbose=>1);

  $lfst->print_att("$basename-l.tfst");
  $rfst->print_att("$basename-r.tfst");
  $filter->print_att("$basename-f.tfst");

  $ilabs->save("$basename.ilabs");
  $olabs->save("$basename.olabs");
  print "All saved.\n";
}
#testims2xb();
#gentfst('lts-ims-german-2');

##-- utility: out2str_packed()
sub out2str_packed {
  if (defined($_[0])) {
    return ('\\n{' . join(',', unpack('S*',$_[0])) . '}');
  }
  return '\\n-undef-';
}

##-- utility: out2str_packed_long()
sub out2str_packed_long {
  if (defined($_[0])) {
    return ('\\n{ ' . join('\\n| ', map { rule2str($rules[$_]) } unpack('S*',$_[0])) . ' }');
  }
  return '\\n-undef-';
}

sub out2str_packed_lts_long {
  if (defined($_[0])) {
    return ('\\n{ ' . join('\\n| ', map { rule2str($lts->{rules}[$_]) } unpack('S*',$_[0])) . ' }');
  }
  return '\\n-undef-';
}

sub out2str_packed_lts_debug {
  if (defined($_[0])) {
    return ("{ " . join(' | ', map { rule2str($lts->{rules}[$_]) } unpack('S*',$_[0])) . " }");
  }
  return '-undef-';
}


## $acpm         = rules2acpm($lts,%args); ##-- scalar context
## ($trie,$acpm) = rules2acpm($lts,%args); ##-- list context
## + %args:
##    rules=>\@rules,
##    reversed=>$bool,
##    which=>\@rule_keys,
##    packout=>$bool,     ##-- whether to pack output function
##    packadd=>$int,
##    complete=>$bool,
##    expandTrie=>$bool,   ##-- BUGGY if true
##    minimizeTrie=>$bool, ##-- BUGGY if true
##    debug=>$bool;
sub crules2acpm {
  my $lts=shift;
  return rules2acpm($lts,rules=>$lts->{rulex},expandTrie=>0,minimizeTrie=>1,@_);
}
sub rulex2acpm {
  my $lts=shift;
  return rules2acpm($lts,rules=>$lts->{rulex},@_);
}
sub rules2acpm {
  my ($lts,%args) = @_;

  vmsg0("subtrie(), ");
  my $trie = subtrie($lts,%args);

  if ($args{expandTrie}) {
    vmsg0("expandTrie(), ");
    our $trie0 = $trie->clone() if ($args{debug});
    $trie = expandTrie($lts,$trie, joinout=>\&Lingua::LTS::_acpm_joinout, %args);
    @{$trie->{chars}}{keys(%{$lts->{letters}}),keys(%{$lts->{specials}})} = undef;
  }
  elsif ($args{minimizeTrie}) {
    vmsg0("minimizeTrie(), ");
    our $trie0 = $trie->clone() if ($args{debug});
    $trie = minimizeTrie($lts,$trie, joinout=>\&Lingua::LTS::_acpm_joinout, %args);
    @{$trie->{chars}}{keys(%{$lts->{letters}}),keys(%{$lts->{specials}})} = undef;
  }

  vmsg0("ACPM->newFromTrie(), ");
  my $acpm = Lingua::LTS::ACPM->newFromTrie($trie,
					    joinout=>\&Lingua::LTS::_acpm_joinout
					   );

  vmsg0("packout(), ");
  packout($acpm,%args) if ($args{packout});


  vmsg0("complete(), ");
  $acpm->complete() if ($args{complete});

  return wantarray ? ($trie,$acpm) : $acpm;
}

##--------------------------------------------------------------
## Convert output id-hashes to packed sorted id-lists

## $trie_or_acpm = packout($trie_or_acpm,%args)
##  + %args: packadd=>$add_to_rulid,  ##-- default: 0
sub packout {
  my ($acpm,%args) = @_;
  my $packadd = $args{packadd} ? $args{packadd} : 0;
  my ($q);
  foreach $q (0..($acpm->{nq}-1)) {
    $acpm->{out}{$q} = pack('S*',
			    (defined($acpm->{out}{$q})
			     ? (map { $_+$packadd }
				sort { $a <=> $b }
				keys(%{$acpm->{out}{$q}}))
			     : qw()));
  }
  return $acpm;
}

##--------------------------------------------------------------
## Subordinate Trie

## $trie = subtrie($lts,%args)
##  + %args:
##    rules=>\@rules,        ##-- default $lts->{rulex}
##    which=>\@rule_keys,
##    reversed=>$bool,
sub subtrie {
  my ($lts,%args) = @_;

  our $trie = Lingua::LTS::Trie->new();
  @{$trie->{chars}}{keys(%{$lts->{letters}}),keys(%{$lts->{specials}})} = undef;

  my $rules = $args{rules} ? $args{rules} : $lts->{rulex};
  my ($q,$r,@rsyms);
  foreach $r (@$rules) {
    @rsyms = map { @$_ } @$r{@{$args{which}}};
    @rsyms = reverse(@rsyms) if ($args{reversed});
    if (defined($q=$trie->a2q(\@rsyms))) {
      $trie->{out}{$q}{$r->{id}}=undef;
    } else {
      $trie->addArray(\@rsyms,{$r->{id}=>undef});
    }
  }

  return $trie;
}

##--------------------------------------------------------------
## Expansion: ACPM

## $expanded_acpm = expandACPM($acpm,\%class2char2undef, %args)
## + requires:
##    $acpm->complete()
## + %args:
##    joinout => \&sub,
##    debug   => $bool,
sub expandACPM {
  my ($acpm,$classes,%args) = @_;
  my $debug = $args{debug};

  ##-- DEBUG
  if ($debug) {
    our $qlabs   = $acpm->gfsmStateLabels(undef,out2str=>\&out2str_ruleids);
    our $qlabs_p = $acpm->gfsmStateLabels(undef,out2str=>undef);
    our $ilabs   = $acpm->gfsmInputLabels();
  }

  ##-- Phase 1: Generate NFA by expanding $classes
  my $goto    = $acpm->{goto};
  my $out     = $acpm->{out};
  my $joinout = $args{joinout};
  my $nfa     = Gfsm::Automaton->new;
  $nfa->is_deterministic(0);
  $nfa->is_transducer(0);
  $nfa->is_weighted(0);
  $nfa->sort_mode(Gfsm::ASMNone);
  $nfa->root(0);

  my ($q,$c,$cc,$clab,$qto);
  my $labs = Gfsm::Alphabet->new;
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
  my $q0     = packids({0=>undef});
  my $id2set = [$q0];
  my $set2id = {$q0=>0};
  my $dgoto  = [];
  my $drgoto = [];
  my $dout   = {};
  my $ai     = Gfsm::ArcIter->new();

  my ($dq,@nqs,$nq,$nqh,$dqtop,$dqto, %c2nq);
  my @fifo = (0);
  while (defined($dq=shift(@fifo))) {
    @nqs = unpack('S*', $id2set->[$dq]);

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
      if (!defined($dqto=$set2id->{$dqtop=packids($nqh)})) {
	$dqto = $set2id->{$dqtop} = scalar(@$id2set);
	push(@$id2set, $dqtop);
	push(@fifo,    $dqto);
      }
      $dgoto->[$dq]{$c} = $dqto;
      $drgoto->[$dqto]  = "-1 $c"; ##-- HACK: rgoto is buggy in determinized ACPM
    }
  }

  ##-- Phase 3: manually construct a new ACPM from the DFA
  my $dacpm = Lingua::LTS::ACPM->new(
				     %$acpm,
				     goto=>$dgoto,
				     rgoto=>$drgoto,
				     out=>$dout,
				     fail=>[],
				     chars=>{
					     map {$_=>undef} (@$laba[1..$#$laba],
							      grep { !exists($classes->{$_}) }
							      keys(%{$acpm->{chars}}))
					    },
				     nq=>scalar(@$id2set),
				     ##-- new
				     id2set=>$id2set,
				     set2id=>$set2id,
				    );
  ##-- DEBUG
  if ($debug) {
    our $set2id_d = { map { ('{'.join(',',unpack('S*',$_)).'}')=>$set2id->{$_} } keys(%$set2id) };
    our $id2set_d = [ map { ('{'.join(',',unpack('S*',$_)).'}')                }     (@$id2set) ];

    our $qlabs_dacpm = Gfsm::Alphabet->new();
    $qlabs_dacpm->insert("q$_~$id2set_d->[$_]".out2str_lts_ruleids($dout->{$_}))
      foreach (0..($dacpm->{nq}-1));

    our $qlabs_dacpm_short = Gfsm::Alphabet->new();
    $qlabs_dacpm_short->insert("q$_~$id2set_d->[$_]".out2str_ruleids_short($dout->{$_}))
      foreach (0..($dacpm->{nq}-1));

    #viewfsm($nfa,lower=>$labs,states=>$qlabs,title=>'NFA',bg=>1);
    #$acpm->viewps(out2str=>\&out2str_lts_ruleids,title=>'ACPM (-expand)',bg=>1);

    #$dacpm->viewps(ilabels=>$labs,states=>$qlabs_dtrie,title=>'ACPM (+expand)',bg=>1);

    print "--DEBUG is ON--\n";
  }

  return $dacpm;
}

##--------------------------------------------------------------
## Minimization (BUGGY)

sub minimizeTrie {
  my ($lts,$trie,%args) = @_;
  my $debug = $args{debug};

  ##-- debug
  our $trie0 = $trie->clone() if ($debug);

  ##-- get suffixes
  my $q2suffs = trie2osuffixes($trie);

  ##-- map suffixes & suffix-sets to Ids
  my $q2suffsetlab = [];
  my $sufflabs    = Gfsm::Alphabet->new;
  my $suffsetlabs = Gfsm::Alphabet->new;
  $sufflabs->insert('', 0);
  $suffsetlabs->insert('', 0);
  my ($i,$suffh,@slabs);
  foreach $i (0..$#$q2suffs) {
    @slabs = map { $sufflabs->insert($_) } keys(%{$q2suffs->[$i]});
    $q2suffsetlab->[$i] = $suffsetlabs->insert(pack('S*', sort {$a<=>$b} @slabs));
  }

  ##-- merge states with identical suffixes
  my $goto = $trie->{goto};
  my $out  = $trie->{out};
  my ($j,$isetlab, $qsrc,$qdst, $c,$goto_qsrc,$goto_qdst, $goto_qsrc_c,$goto_qdst_c);
  my @merge=qw();
  my %badq = qw();
  foreach $i (1..($trie->{nq}-1)) {
    $isetlab = $q2suffsetlab->[$i];
    foreach $j (grep {!exists($badq{$_})} (0..($i-1))) {
      if ($q2suffsetlab->[$j]==$isetlab) {
	##-- merge i=>j
	@merge=($i=>$j);
	while (@merge) {
	  ($qsrc,$qdst) = splice(@merge,0,2);
	  $goto_qsrc = $goto->[$qsrc];
	  $goto_qdst = $goto->[$qdst];
	  $out->{$qdst} = $out->{$qsrc} if (defined($out->{$qsrc}));
	  foreach $c (keys(%$goto_qsrc)) {
	    $goto_qsrc_c = $goto_qsrc->{$c};
	    if (defined($goto_qdst_c=$goto_qdst->{$c})) {
	      if ($goto_qsrc_c < $goto_qdst_c) {
		push(@merge, $goto_qsrc_c=>$goto_qdst_c);
		$goto_qdst->{$c} = $goto_qsrc_c;
	      } else {
		push(@merge, $goto_qdst_c=>$goto_qsrc_c);
		$goto_qsrc->{$c} = $goto_qdst_c;
	      }
	    } else {
	      ##-- no arc defined?
	      $goto_qdst_c->{$c} = $goto_qsrc_c;
	    }
	  }
	}
	$badq{$i} = $j;
	last;
      }
    }
  }

  ##-- renumber trie states
  my $dtrie = ref($trie)->new(cw=>$trie->{cw},chars=>$trie->{chars});
  my $dgoto = $dtrie->{goto};
  my $drgoto = $dtrie->{rgoto};
  my $dout  = $dtrie->{out};
  my $nq = 1;
  my %q2dq = (0=>0);
  my ($q,$gotoq,$gotodq,$qto,$dqto);
  foreach $q (grep { !exists($badq{$_}) } (0..($trie->{nq}-1))) {
    $gotoq  = $goto->[$q];
    $dq     = $q2dq{$q};
    $dout->{$dq} = $trie->{out}{$q} if (defined($trie->{out}{$q}));
    while (($c,$qto)=each(%$gotoq)) {
      $qto  = $badq{$qto} if (defined($badq{$qto}));
      $dqto = $q2dq{$qto} = $nq++ if (!defined($dqto=$q2dq{$qto}));
      $dgoto->[$dq]{$c} = $dqto;
      $drgoto->[$dqto] = "$dq $c"; ##-- HACK
    }
  }
  $dtrie->{nq} = $nq;

  if ($debug) {
    ##--ARGH
    # DB<390> x map { out2str_packed_lts_debug($_) } @{$racpm0->{out}}{@{$racpm0->s2path($wr='#netnu#')}}
    # 0  '{  }'
    # 1  '{ 87:[#:] }'
    # 2  '{ 39:[n:n] }'
    # 3  '{ 5:#C*[en:e:n]# | 6:[en:@n]# | 24:[e:e:]C | 25:[e:@] }'
    # 4  '{ 47:[t:t] }'
    # 5  '{ 86:[z:ts] }'
    # 6  '{ 64:[u:u:] }'
    # 7  '{ 87:[#:] }'

    #$trie->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'Trie');
    #$dtrie->viewps(out2str=>\&out2str_lts_ruleids,bg=>1,title=>'DTrie');
    print "--DEBUG ON--\n";
  }

  return $dtrie;
}

sub trie2osuffixes {
  my $trie = shift;
  my $q2suffs = [];
  my $rgoto = $trie->{rgoto};
  my $out = $trie->{out};
  my @fifo = (map { ($_=>(':'.join('|',sort {$a<=>$b} keys(%{$out->{$_}})))) } keys(%$out));
  my ($qto,$suff,$qfrom,$c);
  while (@fifo) {
    ($qto,$suff) = splice(@fifo,0,2);
    $q2suffs->[$qto]{$suff} = undef;
    next if ($qto==0);
    ($qfrom,$c)  = split(/ /, $rgoto->[$qto]);
    push(@fifo, $qfrom=>($c.$suff));
  }
  return $q2suffs;
}

sub trie2suffixes {
  my $trie = shift;
  my $q2suffs = [];
  my $rgoto = $trie->{rgoto};
  my @fifo = (map {($_=>'')} keys(%{$trie->{out}}));
  my ($qto,$suff,$qfrom,$c);
  while (@fifo) {
    ($qto,$suff) = splice(@fifo,0,2);
    $q2suffs->[$qto]{$suff} = undef;
    next if ($qto==0);
    ($qfrom,$c)  = split(/ /, $rgoto->[$qto]);
    push(@fifo, $qfrom=>($c.$suff));
  }
  return $q2suffs;
}


##--------------------------------------------------------------
## Expansion: Trie

## --> BUGGY

##-- $expandedTrie = expandTrie($lts,$trie,%args)
## + expand class-arcs in $trie
## + %args:
##    joinout=>\&sub,
sub expandTrie {
  my ($lts,$trie,%args) = @_;

  my $debug   = exists($args{debug}) ? $args{debug} : 1;
  my $classes = $lts->{classes};
  my $goto    = $trie->{goto};        ##-- literal (array)
  my $out     = $trie->{out};
  my $joinout = $args{joinout};
  my $nfa     = Gfsm::Automaton->new;
  $nfa->is_deterministic(0);
  $nfa->is_transducer(0);
  $nfa->is_weighted(0);
  $nfa->sort_mode(Gfsm::ASMNone);
  $nfa->root(0);

  ##-- DEBUG
  if ($debug) {
    our $qlabs   = $trie->gfsmStateLabels(undef,out2str=>\&out2str_ruleids);
    our $qlabs_p = $trie->gfsmStateLabels(undef,out2str=>undef);
    our $ilabs   = $trie->gfsmInputLabels();
  }

  ##-- Phase 1:
  ##   + generate non-deterministic fsa
  my ($q,$c,$clab,$qto);
  my $labs = Gfsm::Alphabet->new;
  $labs->insert('<eps>',0);
  foreach $q (0..($trie->{nq}-1)) {
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

  ##-- Phase 2: determinize
  my $laba = $labs->asArray();
  my $q0 = packids({0=>undef});
  my $set2id = {$q0=>0};
  my $id2set = [$q0];
  my $dgoto  = [];
  my $drgoto = [];
  my $dout   = {};
  my @fifo = qw(0);
  my $ai = Gfsm::ArcIter->new();
  my ($dq,@nqs,$nq,$nqh,$dqtop,$dqto, %c2nq);
  while (defined($dq=shift(@fifo))) {
    @nqs = unpack('S*',$id2set->[$dq]);

    ##-- join output
    if (defined($joinout)) {
      foreach $nq (@nqs) {
	$dout->{$dq} = $joinout->($dout->{$dq}, $out->{$nq});
      }
    }

    ##-- get transition map
    %c2nq = qw();
    foreach $nq (@nqs) {
      for ($ai->open($nfa,$nq); $ai->ok; $ai->next) {
	$c2nq{$laba->[$ai->lower]}{$ai->target} = undef;
      }
    }

    ##-- instantiate output states
    while (($c,$nqh)=each(%c2nq)) {
      if (!defined($dqto=$set2id->{$dqtop=packids($nqh)})) {
	$dqto = $set2id->{$dqtop} = scalar(@$id2set);
	push(@$id2set, $dqtop);
      }
      $dgoto->[$dq]{$c} = $dqto;
      $drgoto->[$dqto]  = "$dq $c"; ##-- HACK

      ##-- enqueue
      push(@fifo,$dqto);
    }
  }

  ##-- Phase 3: manually construct a trie
  my $dtrie = Lingua::LTS::Trie->new(
				     goto=>$dgoto,
				     rgoto=>$drgoto,
				     out=>$dout,
				     chars=>{ map {$_=>undef} @$laba[1..$#$laba] },
				     nq=>scalar(@$id2set),
				     cw=>$trie->{cw},
				     epsilon=>$trie->{epsilon},
				     ##-- new
				     id2set=>$id2set,
				     set2id=>$set2id,
				    );
  ##-- DEBUG
  if ($debug) {
    our $qlabs_dtrie = Gfsm::Alphabet->new();
    $qlabs_dtrie->insert("q$_"
		       ."~{".join(',', unpack('S*', $id2set->[$_])).'}'
		       .out2str_lts_ruleids($dout->{$_}))
      foreach (0..($dtrie->{nq}-1));

    our $qlabs_dtrie_short = Gfsm::Alphabet->new();
    $qlabs_dtrie_short->insert("q$_"
			       ."~{".join(',', unpack('S*', $id2set->[$_])).'}'
			       .out2str_ruleids_short($dout->{$_}))
      foreach (0..($dtrie->{nq}-1));

    #viewfsm($nfa,lower=>$labs,states=>$qlabs,title=>'NFA',bg=>1);
    #$trie->viewps(out2str=>\&out2str_lts_ruleids,title=>'Trie (-det)',bg=>1);

    #$dtrie->viewps(ilabels=>$labs,states=>$qlabs_dtrie,title=>'DTrie',bg=>1);

    print "--DEBUG is ON--\n";
  }

  return $dtrie;
}

##-- utility: $packed = packids(\%id2x)
sub packids { return pack('S*', sort {$a<=>$b} keys(%{$_[0]})); }

##-- utility: out2str_ruleids_short
sub out2str_ruleids_short {
  return (defined($_[0])
	  ? ('='.join('|',sort { $a<=>$b } keys(%{$_[0]})))
	  : '');
}

##--------------------------------------------------------------
## Test: lookup (FST)

## undef = fstlkp($w, $fst,%args)
##  + %args:
##      lower=>$ilabs,
##      upper=>$olabs,
sub fstlkp {
  my ($w,$fst,%args) = @_;

  $w = '' if (!defined($w));

  ##-- FST
  $fst = $main::fst if (!defined($fst));
  my $ilabs = $args{lower} ? $args{lower} : $main::iolabs;
  my $olabs = $args{upper} ? $args{upper} : $main::iolabs;
  @wlabs  = grep { defined($_) } @{$ilabs->asHash}{(($lts->{implicit_bos} ? '#' : qw()),
						    split(//,$w),
						    ($lts->{implicit_eos} ? '#' : qw()))};
  our $result = $fst->lookup(\@wlabs);
  our $paths  = $result->paths(Gfsm::LSUpper);

  our @phones_lts = $lts->apply_word($w);
  our @phones_fst = map { [@{$olabs->asArray}[@{$_->{hi}}]] } @$paths;
  print
    ("w=\"$w\"\n",
     "\tLTS: ", join(' ', '(', @phones_lts, ')'), "\n",
     (@phones_fst
      ? (
	 map {
	   ("\tFST: ", join(' ', '(', @{$phones_fst[$_]}, ')'), "\n")
	 } (0..$#phones_fst)
	)
      : "\tFST: -EMPTY-\n"),
    );
}

##--------------------------------------------------------------
## Debug: string reverse
sub sreverse { return join('',reverse(split(//,$_[0]))); }

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

##-- DEBUG
sub nfasuccs {
  my ($nfa,$q,$lab) = @_;
  my $ai = Gfsm::ArcIter->new($nfa,$q);
  my @succs = qw();
  for ( ; $ai->ok; $ai->next) {
    push(@succs, $ai->target) if ($ai->lower == $lab);
  }
  return @succs;
}

sub nfapaths {
  my ($nfa,@labs) = @_;
  my @paths = qw();
  my @configs = ([0,$nfa->root]);
  my ($cfg,$pos,@path,@succs);
  while (defined($cfg=shift(@configs))) {
    ($pos,@path) = @$cfg;
    if ($pos > $#labs) { push(@paths,[@path]); next; }
    @succs = nfasuccs($nfa,$path[$#path], $labs[$pos]);
    push(@configs, map { [$pos,@path,$_] } @succs);
  }
  return @paths;
}

##-- DEBUG
sub rdebug1 {
  my $wr = shift;
  our ($xq,$dq,$dq0);
  print STDERR
    (join(", ",
	  "wr='$wr'",
	  ("xq=".($xq=$xracpm->s2q($wr))),
	  ("xout=".out2str_packed_lts_debug($xracpm->{out}{$xq})),
	  ("dq=".($dq=$racpm->s2q($wr))),
	  ("dout=".out2str_packed_lts_debug($racpm->{out}{$dq})),
	  ("dq0=".($dq0=$racpm0->s2q($wr))),
	  ("dout0=".out2str_packed_lts_debug($racpm0->{out}{$dq0})),
	 ),
     "\n");
}
#

##--- main: dummy
foreach $i (0..5) {
  print "--dummy[$i]--\n";
}
