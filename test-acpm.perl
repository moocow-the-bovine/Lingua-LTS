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
		{lhs=>[qw(b a)], in=>[qw(b b)], rhs=>[qw(a b)], out=>['B'] },  ##-- ba[bb:B]ab
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
sub testims   { testlts('lts-ims-german.lts'); }
sub testimsx  { testltsx('lts-ims-german.lts'); }
sub testims2  { testlts('lts-ims-german-2.lts'); }
sub testims2x { testltsx('lts-ims-german-2.lts'); }

##--------------------------------------------------------------
## Debug: FSM Viewing

sub viewtrie  { $trie->viewps(title=>'Lingua::LTS::Trie',bg=>1,states=>$tqlabs_d,@_); }

sub viewacpm  { $acpm->viewps(title=>'Lingua::LTS::ACPM_debug',bg=>1,states=>$qlabs_d,@_); }
sub viewacpm0 { $acpm0->viewps(title=>'Lingua::LTS::ACPM_0',bg=>1,states=>$qlabs_d,@_); }

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

  our $tqlabs_d = $trie->gfsmStateLabels;
  while (($q,$out) = each(%{$trie->{out}})) {
    my $oldlab = $tqlabs_d->find_key($q);
    $oldlab =~ s/\=HASH.*$/\=/;
    $tqlabs_d->insert($oldlab.join('|', map { rule2str($rules[$_]) } sort {$a<=>$b} keys(%$out)), $q);
  }
}
#test1;
#gentrie;

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

  our $qlabs_d = $acpm->gfsmStateLabels;
  while (($q,$out) = each(%{$acpm->{out}})) {
    my $oldlab = $qlabs_d->find_key($q);
    $oldlab =~ s/\=HASH.*$/\=/;
    $qlabs_d->insert($oldlab.join('|', map { rule2str($rules[$_]) } sort {$a<=>$b} keys(%$out)), $q);
  }
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
#lkptest('ababbab');
#lkptest('babbab');
##--
testims2x;
genacpm;
#lkptest('#schied#');
lkptest('#unterschied#');

##--- main: dummy
foreach $i (0..5) {
  print "--dummy[$i]--\n";
}
