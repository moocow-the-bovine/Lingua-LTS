## File: Lingua::LTS.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: festival-style letter-to-sound rules


package Lingua::LTS;
#use Lingua::LTS::Automaton;
use Lingua::LTS::Trie;
use Lingua::LTS::ACPM;
use strict;

use IO::File;
use Carp;

##==============================================================================
## Constants
##==============================================================================

our $VERSION = 0.02;

##-- always specials
our @SPECIALS = ('#');
our %SPECIALS = (map { $_=>undef } @SPECIALS);

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure:
##     ##-- basic stuff
##     classes => {$name=>\%expansions, ...},
##     rules   => \@rules         ##-- rules with class names in context components
##                = [ {lhs=>$lhs1, in=>$in1, out=>$out1, rhs=>$rhs1}, ... ],
##     ##
##     ##-- derived stuff
##     rulex   => \@exanded_rules, ##-- no class names in context components; req: expand_rules()
##     phones0  => \%phones,      ##-- pseudo-set: phones requested in .lts file
##     letters => \%letters,      ##-- pseudo-set: req: expand_alphabet()
##     phones  => \%phones,       ##-- pseudo-set: req: expand_alphabet()
##     specials => \%specials,    ##-- pseudo-set: req: expand_alphabet()
##     ##
##     ##-- Options
##     implicit_bos => $bool,     ##-- default=1
##     implicit_eos => $bool,     ##-- default=1
##     ##
##     ##-- debugging
##     apply_verbose => $bool,
##     apply_warn=>$bool,
##
sub new {
  my $that = shift;
  return bless({
		classes=>{},
		rules=>[],
		letters=>{},
		phones=>{},
		phones0=>{},
		specials=>{%SPECIALS},
		apply_verbose=>0,
		apply_warn=>1,
		implicit_bos=>1,
		implicit_eos=>1,
		@_
	       }, ref($that)||$that);
}

##==============================================================================
## Methods: I/O
##==============================================================================

##--------------------------------------------------------------
## Methods: I/O: Input: .lts

## $obj = $CLASS_OR_OBJ->load($filename_or_fh)
##  + File Syntax:
##    LTS_FILE ::= LTS_LINE*
##    LTS_LINE ::= ( BLANK | COMMENT | PHON | CLASS | IGNORE | RULE ) "\n"
##    BLANK    ::= (whitespace)
##    COMMENT  ::= ";" (anything)
##    PHON     ::= "phon" PHONSTRINGS
##    CLASS    ::= "class" CLASS_NAME CHARS
##    CLASS_NAME ::= (string)
##    CHARS      ::= ((string) | "#") *
##    IGNORE     ::= "ignore" CHARS
##    RULE       ::= RULE_LHS "[" RULE_IN "]" RULE_RHS "=" RULE_OUT
sub load {
  my ($lts,$file) = @_;
  $lts = $lts->new() if (!ref($lts));

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak(__PACKAGE__, "::load(): open failed for '$file': $!") if (!$fh);

  my (@phones,$cname,@cchars, $lhs,$in,$rhs,$out);
  while (<$fh>) {
    chomp;
    s/[^\\]\;.*//;     ##-- ignore comments
    next if (/^\s*$/); ##-- ... and blank lines
    s/\\(.)/$1/g;      ##-- un-escape

    if (/^\s*phon\s+(.*\S)\s*/) {
      @phones = split(/\s+/,$1);
      @{$lts->{phones0}}{@phones} = undef;
    }
    elsif (/^\s*class\s+(\S+)\s+(.*\S)\s*/) {
      $cname  = $1;
      @cchars = split(/\s+/,$2);
      $lts->{classes}{$cname} = {} if (!defined($lts->{classes}{$cname}));
      @{$lts->{classes}{$cname}}{@cchars} = undef;
    }
    elsif (/^\s*ignore\s+(.*\S)\s*/) {
      $lts->{classes}{$cname} = {};
    }
    elsif (/^\s*([^\[]*)\[([^\]]*)\]([^\=]*)=(.*)/) {
      ($lhs,$in,$rhs,$out) = ($1,$2,$3,$4);
      push(@{$lts->{rules}},
	   {
	    id=>scalar(@{$lts->{rules}}),
	    lhs=>[grep { defined($_) && $_ ne '' } split(/\s+/,$lhs)],
	    in=>[grep { defined($_) && $_ ne '' } split(/\s+/,$in)],
	    rhs=>[grep { defined($_) && $_ ne '' } split(/\s+/,$rhs)],
	    #out=>[map { "=$_" } grep { defined($_) && $_ ne '' } split(/\s+/,$out)],
	    out=>[map { "$_" } grep { defined($_) && $_ ne '' } split(/\s+/,$out)],
	   });
    }
    else {
      warn(__PACKAGE__, "::load(): could not parse line '$_' -- ignoring\n");
    }
  }

  $fh->close if (!ref($file));
  return $lts;
}

##--------------------------------------------------------------
## Methods: I/O: Output: symbols

## $lts = $lts->save_symbols($symbols_filename_or_fh)
##  + requires: lts_expaned_alphabet()
sub save_symbols {
  my ($lts,$file) = @_;

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak(__PACKAGE__, "::save_symbols(): open failed for '$file': $!") if (!$fh);

  ##-- print specials
  $lts->print_symbols_lines($fh,'Special',[
					   @SPECIALS,
					   (grep { !exists($SPECIALS{$_}) }
					    sort(keys(%{$lts->{specials}})))
					  ]);
  $fh->print("\n");

  ##-- print letters [literals]
  $lts->print_symbols_lines($fh,'Letter',[
					  sort(keys(%{$lts->{letters}}))
					 ]);
  $fh->print("\n");

  ##-- print phones (temporary)
  $lts->print_symbols_lines($fh,'Phon',   [map { $_ } sort(keys(%{$lts->{phones}}))]);
  $fh->print("\n");

  ##-- print classes
  my $classes = $lts->{classes};
  my ($c);
  foreach $c (sort(keys(%$classes))) {
    $lts->print_symbols_lines($fh,"Class$c", [              sort(keys(%{$classes->{$c}}))]);
    $fh->print("\n");
  }
  $fh->print("\n");

  $fh->close if (!ref($file));
  return $lts;
}

## undef = $lts->print_symbols_lines($fh,$classname,\@symbols)
sub print_symbols_lines {
  my ($lts,$fh) = (shift,shift);
  $fh->print($lts->symbols_lines(@_));
}

## @lines = $lts->symbols_lines($classname,\@symbols)
sub symbols_lines {
  my ($lts,$class,$syms) = @_;
  my @lines = qw();
  my ($line);
  while (@$syms) {
    $line = "$class\t";
    while (@$syms && length($line) + length($syms->[0]) < 80) {
      $line .= ' '.shift(@$syms);
    }
    push(@lines,$line."\n");
  }
  return @lines;
}

##==============================================================================
## Methods: index generation: ACPM
##==============================================================================

## $acpm = $lts->toACPM(%args)
##  + %args:
##     complete=>$bool  # complete the ACPM
##  + requires: expand_rules(), expand_alphabet()
##  + populates: $lts->{acpm}
*compile_acpm = *toACPM;
sub toACPM {
  my ($lts,%args) = @_;

  ##-- step 1: generate trie
  my $trie = Lingua::LTS::Trie->new;
  my ($r,%rstrs,$rstr,$q);
  foreach $r (@{$lts->{rulex}}) {
    %rstrs = map { $_=>join('',@{$r->{$_}}) } (qw(lhs in out rhs));
    $rstr  = join('', @rstrs{qw(lhs in rhs)});
    if (defined($q=$trie->s2q($rstr))) {
      $trie->{out}{$q}{$r->{id}} = undef;
    } else {
      $q = $trie->add($rstr,{$r->{id}=>undef});
    }
  }

  ##-- step 1b: add EOS pseudo-rule (hack)
  if ($lts->{implicit_eos} && !defined($trie->{goto}[0]{'#'})) {
    $trie->add('#',undef);
  }

  ##-- step 2: generate ACPM
  my $acpm = $lts->{acpm} = Lingua::LTS::ACPM->newFromTrie($trie,joinout=>\&_acpm_joinout);
  $acpm->complete() if ($args{complete});

  return $acpm;
}

## \%idhash = _acpm_joinout($hash1_or_undef, $hash2_or_undef)
sub _acpm_joinout {
  if (defined($_[0])) {
    @{$_[0]}{keys %{$_[1]}} = undef if (defined($_[1]));
    return $_[0];
  }
  return {%{$_[1]}} if (defined($_[1]));
}


## $labs = $lts->gfsmLabels
##   + requires: $lts->expand_alphabet()
sub gfsmLabels {
  my $lts = shift;
  my $labs = Gfsm::Alphabet->new;
  $labs->insert($_) foreach ('<epsilon>', map { sort keys %$_ } @$lts{qw(specials letters phones)});
  return $labs;
}

## @symLines = $lts->gfsmSymbolsLines()
##   + requires: $lts->expand_alphabet()
sub gfsmSymbolsLines {
  my $lts = shift;
  return ($lts->symbols_lines('Special', $lts->{specials}), "\n",
	  $lts->symbols_lines('Letter', $lts->{letters}), "\n",
	  $lts->symbols_lines('Phon', $lts->{phones}), "\n");
}

## $gfsmFST = $lts->gfsmFST(%args)
##  + requires: $lts->{acpm} (NOT complete)
##  + %args:
##     ilabels=>$ilabs, ##-- default = $lts->gfsmLabels()
##     olabels=>$olabs, ##-- default = $ilabs
*gfsmFST = \&gfsmTransducer;
sub gfsmTransducer {
  my ($lts,%args) = @_;
  my $ilabs = $args{ilabels} ? $args{ilabels} : $lts->gfsmLabels();
  my $olabs = $args{olabels} ? $args{olabels} : $ilabs;
  my $acpm  = $lts->{acpm};
  my $goto  = $acpm->{goto};
  my $rgoto = $acpm->{rgoto};

  print STDERR (ref($lts), "::gfsmTransducer(): mapping output rules to states (backward)... ")
    if ($args{verbose});

  ##-- Phase 1:
  ##    + map output rules to the states at which they would apply,
  ##      factoring out input and right-hand side

  ##-- $q=>{ pack('S4L', ${in_begin}, ${in_end}, ${rhs_end}, ${nread}, ${rulid}), ... }
  ##   + all positions are encoded as substr() indices in address($q)
  my $q2rulpos = $lts->{q2rulpos} = [];
  my $rules = $lts->{rules};
  my ($q,$qout,$rulid,$rul, $lenL,$lenLI,$lenLIR,$nread,$r);
  while (($q,$qout)=each(%{$acpm->{out}})) {
    foreach $rulid (keys %$qout) {
      $rul    = $rules->[$rulid];
      $lenL   = @{$rul->{lhs}};
      $lenLI  = $lenL + @{$rul->{in}};
      $lenLIR = $lenLI + @{$rul->{rhs}};

      ##-- back up, adopting backwards
      for ($nread=$lenLIR, $r=$q; $nread > 0 && defined($r); $nread--) {
	$q2rulpos->[$r]{pack('S4L', $lenL, $lenLI, $lenLIR, $nread, $rulid)} = undef;
	$r = (split(/ /,$rgoto->[$r]))[0];
      }
    }
  }

  print STDERR ("done.\n",
		ref($lts), "::gfsmTransducer(): eliminating overlap & inheriting forward... ")
    if ($args{verbose});

  ##-- Phase 2:
  ##    + eliminate redundant rules (overlap)
  ##    + inherit *completed* rules from $q to $qto on ($q --$c--> $qto)
  my @fifo = (0);
  my ($rp,$c,$qto, @rps,$badi,%rpibad);
  my ($rpi1,$lenL1,$lenLI1,$lenLIR1,$nread1,$rulid1,$begin1,$end1);
  my ($rpi2,$lenL2,$lenLI2,$lenLIR2,$nread2,$rulid2,$begin2,$end2);
  while (defined($q=shift(@fifo))) {
    ##-- eliminate redundant rules (overlap) in $q2rulpos
    @rps    = defined($q2rulpos->[$q]) ? keys(%{$q2rulpos->[$q]}) : qw();
    %rpibad = qw();
    foreach $rpi1 (1..$#rps) {
      next if (exists($rpibad{$rpi1}));
      ($lenL1,$lenLI1,$lenLIR1,$nread1,$rulid1) = unpack('S4L', $rps[$rpi1]);
      next if ($nread1 < $lenLIR1);   ##-- unfinished: keep it
      ($begin1,$end1) = ($lenL1-$nread1, $lenLI1-$nread1);
      foreach $rpi2 (0..($rpi1-1)) {
	next if (exists($rpibad{$rpi2}));
	($lenL2,$lenLI2,$lenLIR2,$nread2,$rulid2) = unpack('S4L', $rps[$rpi2]);
	next if ($nread2 < $lenLIR2); ##-- unfinished: keep it
	($begin2,$end2) = ($lenL2-$nread2, $lenLI2-$nread2);

	if ($begin1==$begin2 && $rulid1 != $rulid2) {
	  ##-- identical input begin position: prefer best rulid
	  $badi = $rulid1 < $rulid2 ? $rpi2 : $rpi1;
	  $rpibad{$badi} = undef;
	  delete($q2rulpos->[$q]{$rps[$badi]});
	}
	elsif ($begin1 < $begin2 && $end1 > $begin2) {
	  ##-- overlap: rule1 << rule2
	  $rpibad{$rpi2} = undef;
	  delete($q2rulpos->[$q]{$rps[$rpi2]});
	}
	elsif  ($begin2 < $begin1 && $end2 > $begin1) {
	  ##-- overlap: rule2 << rule1
	  $rpibad{$rpi1} = undef;
	  delete($q2rulpos->[$q]{$rps[$rpi1]});
	}
      }
    }
    @rps = keys(%{$q2rulpos->[$q]}) if (@rps);

    ##-- process daughter states ($q --$c--> $qto)
    while (($c,$qto)=each(%{$goto->[$q]})) {
      push(@fifo,$qto) if ($qto != 0);
      ##-- adopt completed rules forward from $q to $qto: buffer output
      foreach $rp (@rps) {
	($lenL,$lenLI,$lenLIR,$nread,$rulid) = unpack('S4L', $rp);
	next if ($nread < $lenLIR); ##-- don't adopt incomplete rules
	$q2rulpos->[$qto]{pack('S4L', $lenL,$lenLI,$lenLIR,$nread+1,$rulid)} = undef;
      }
    }
  }

  print STDERR ("done.\n",
		ref($lts), "::gfsmTransducer(): completing delta()...",
		#"\n",
	       )
    if ($args{verbose});

  ##-- Phase 3
  ##    + complete goto()
  ##      struct: $delta->[$q] = { $c=>join(' ', $qto, @output), ... }
  ##
  my $delta = [];
  my $outF  = []; ##-- $outF->[$q] => @output_on_eos
  my @chars = keys(%{$acpm->{chars}});
  push(@chars, '#')
    if (!exists($acpm->{chars}{'#'}) && ($lts->{implicit_bos} || $lts->{implicit_eos}));
  my $fail = $acpm->{fail};
  my ($gotoq,$deltaq, @qrps, $rp1,$rp2, $q2,$goto_qc,@rps_qc,@out_qc);
  my (@qin,$cin);
  my $nQ=0;
  foreach $q (0..($acpm->{nq}-1)) {
    print STDERR '.' if ($args{verbose} && $nQ++ % 1000 == 0);

    ##-- input arcs: $q --a:eps--> $goto->[$q]{$a}
    $gotoq  = $goto->[$q];
    $deltaq = $delta->[$q] = { map { $_=>$gotoq->{$_} } keys(%$gotoq) };

    ##-- get safe output for $q
    @qrps =
      (
       sort { $a->{begin} <=> $b->{begin} }
       map {
	 ($lenL1,$lenLI1,$lenLIR1,$nread1,$rulid1) = unpack('S4L', $_);
	 ($nread1 >= $lenLIR1
	  ? {
	     lenL=>$lenL1,
	     lenLI=>$lenLI1,
	     lenLIR=>$lenLIR1,
	     nread=>$nread1+1,
	     rulid=>$rulid1,
	     begin=>($lenL1-$nread1-1),
	     end=>($lenLI1-$nread1-1),
	    }
	  : qw())
       } keys(%{$q2rulpos->[$q]})
      );

    $outF->[$q] = [map { @{$lts->{rules}[$_->{rulid}]{out}} } @qrps];

    ##-- get input (target) for q
    @qin = map { @{$lts->{rules}[$_->{rulid}]{in}} } @qrps;

    ##-- failure arcs: $q --eps:$out_qc--> $q2 --$c:eps--> $goto->[$q2]{$c}=$goto_qc
    foreach $c (grep { !exists($deltaq->{$_}) } @chars) {
      $q2 = $fail->[$q];
      ##-- follow fail-paths for all completed inputs of $q
      foreach $cin (@qin[1..$#qin]) {
	$q2 = $fail->[$q2] while (!defined($goto_qc=$goto->[$q2]{$cin}));
	$q2 = $goto_qc;
      }
      ##-- follow failure path for completed input path
      $q2 = $fail->[$q2] while (!defined($goto_qc=$goto->[$q2]{$c}));

      ##-- get allowable configurations @rps_qc for $q --$c:???--> $q2
      @rps_qc = qw();
    RP1:
      foreach $rp1 (@qrps) {
	($begin1,$end1,$rulid1) = @$rp1{qw(begin end rulid)};
	foreach $rp2 (keys(%{$q2rulpos->[$goto_qc]})) {
	  ($lenL2,$lenLI2,$lenLIR2,$nread2,$rulid2) = unpack('S4L', $rp2);
	  next if ($nread2 < $lenLIR2); ##-- ignore incompletely read rules in fail sink state (?)
	  ($begin2,$end2) = ($lenL2-$nread2, $lenLI2-$nread2);

	  if ($begin1==$begin2 && $end1==$end2) {
	    ##-- identical match: prefer best rulid
	    next RP1 if ($rulid2 < $rulid1);
	  }
	  elsif ($begin1 < $begin2 && $end1 > $begin2) {
	    ##-- overlap: rule1 << rule2
	    warn("\n",
		 ref($lts), "::gfsmTransducer(): Error: nonsensical overlap for q=$q, c=$c, qto=$goto_qc: ",
		 "\n  > r1={$begin1..$end1} ~ $rulid1 : ", rule2str($lts->{rules}[$rulid1]),
		 "\n  > r2={$begin2..$end2} ~ $rulid2 : ", rule2str($lts->{rules}[$rulid2]),
		 "\n  > ")
	  }
	  elsif ($begin2 < $begin1 && $end2 > $begin1) {
	    ##-- overlap: rule2 << rule1
	    next RP1;
	  }
	}
	push(@rps_qc, $rp1);
      }

      ##-- get contiguous output for allowable configurations
      @out_qc = map {
	@{$lts->{rules}[$_->{rulid}]{out}}
      } sort { $a->{begin} <=> $b->{begin} } @rps_qc;

      ##-- mark output arc
      $deltaq->{$c} = join(' ',$goto_qc,@out_qc);
    }
  }

  print STDERR ("done.\n",
		ref($lts), "::gfsmTransducer(): constructing FST... ")
    if ($args{verbose});

  ##-- Phase 4: FST construction
  my $fst = Gfsm::Automaton->new();
  $fst->is_transducer(1);
  $fst->is_weighted(0);
  $fst->root($fst->ensure_state(0));

  ##-- add designated EOS state
  my ($qeos,$qmax,$eoslab);
  $qeos = $acpm->{nq};
  $qmax = $qeos+1;
  $fst->add_state($qeos);
  $fst->is_final($qeos,1);

  ##-- add all states, arcs
  my ($delta_qc, $i, $clab);
  foreach $q (0..($acpm->{nq}-1)) {
    ##-- add eos arcs: $q --eps:$outF[$q]--> $qeos
    #if (!$lts->{implicit_eos}) {
      @out_qc = @{$outF->[$q]};
      $fst->add_arc($q, ($#out_qc <= 0 ? $qeos                         : ($qmax++)),
		    0,  ($#out_qc >= 0 ? $olabs->get_label($out_qc[0]) : 0),
		    0);
      foreach $i (1..$#out_qc) {
	$fst->add_arc($qmax-1, ($i==$#out_qc ? $qeos : ($qmax++)),
		      0,       $olabs->get_label($out_qc[$i]),
		      0);
      }
    #}

    ##-- add arcs $q --$c:$out_qa--> $q2
    $deltaq = $delta->[$q];
    while (($c,$delta_qc)=each(%$deltaq)) {
      ($qto,@out_qc) = split(/ /, $delta_qc);
      $clab = $ilabs->get_label($c);

      ##-- handle usual arcs
      $fst->add_arc($q,    ($#out_qc <= 0 ? $qto                          : ($qmax++)),
		    $clab, ($#out_qc >= 0 ? $olabs->get_label($out_qc[0]) : 0),
		    0);
      foreach $i (1..$#out_qc) {
	$fst->add_arc($qmax-1, ($i==$#out_qc ? $qto : ($qmax++)),
		      0,       $olabs->get_label($out_qc[$i]),
		      0);
      }
    }
  }

  print STDERR "done.\n"
    if ($args{verbose});

  $fst->arcsort(Gfsm::ASMLower()) if ($args{dosort} || !exists($args{dosort}));
  return $fst;
}



##==============================================================================
## Methods: indexed application: ACPM
##==============================================================================

## @phones = $lts->apply_indexed($word)
##  + get phones for string $word
##  + requires: compile_acpm()
*apply_indexed = *apply_word_indexed = \&apply_word_indexed_acpm;
sub apply_word_indexed_acpm {
  my ($lts,$word) = @_;
  $word = '#'.$word if ($lts->{implicit_bos});
  $word = $word.'#' if ($lts->{implicit_eos});

  ##-- get matches
  my @matches = $lts->{acpm}->matches($word);
  return $lts->matches2phones($word,\@matches);
}

## @phones = $lts->apply_indexed($word)
##  + get phones for string $word
##  + requires: $lts->{acpm}, $lts->{gacpm}, $lts->{glabs}
sub apply_word_indexed_acpm_gfsm {
  my ($lts,$word) = @_;
  $word = '#'.$word if ($lts->{implicit_bos});
  $word = $word.'#' if ($lts->{implicit_eos});

  ##-- get matches
  my @wlabs   = grep { defined($_) } @{$lts->{glabs}->asHash}{split(//,$word)};
  my $qids    = $lts->{gacpm}->find_prefix_states(\@wlabs,[]);
  my @matches = @{$lts->{acpm}{out}}{@$qids};
  return $lts->matches2phones($word,\@matches);
}


## @phones = $lts->matches2phones($word,\@matches)
sub matches2phones {
  my ($lts,$word,$matches) = @_;

  ##-- adjust match positions (compensate for rhs,lhs)
  my @best    = qw();
  my ($i,$ruli,$rul,$besti);
  foreach $i (grep { defined($matches->[$_]) } (0..$#$matches)) {
    foreach $ruli (keys %{$matches->[$i]}) {
      $rul   = $lts->{rules}[$ruli];
      $besti = $i - @{$rul->{rhs}} - @{$rul->{in}} + 1;
      $best[$besti] = $ruli if (!defined($best[$besti]) || $best[$besti] > $ruli);
    }
  }

  ##-- get output phones
  my @phones = qw();
  for ($i=0; $i <= $#best; $i++) {
    next if (!defined($best[$i]));
    if (defined($rul=$lts->{rules}[$best[$i]])) {
      if ($lts->{apply_verbose}) {
	my $vword = substr($word,0,($i-1)).'_'.substr($word,($i-1),length($word)-$i+1);
	print STDERR "Match: \'$vword\' matches rule $rul->{id}: ", rule2str($rul), "\n";
      }
      push(@phones, @{$rul->{out}});
      $i += $#{$rul->{in}};
    }
  }

  return @phones;
}


##==============================================================================
## Methods: Rule Expansion
##==============================================================================

## $lts = $lts->expand_rules()
##  + expands all class names in $lts->{rules} into $lts->{rulex}
sub expand_rules {
  my $lts = shift;
  my $rules   = $lts->{rules};
  my $rulex   = $lts->{rulex} = [ @$rules ];

  my ($i,$r,$lhs,$rhs,@new_lhs,@new_rhs,@newrules);

  for ($i=0; $i < @$rulex; $i++) {
    $r = $rulex->[$i];
    @new_lhs  = $lts->expand_side($r->{lhs});
    @new_rhs  = $lts->expand_side($r->{rhs});
    @newrules = qw();
    foreach $lhs (@new_lhs) {
      foreach $rhs (@new_rhs) {
	push(@newrules, { %$r, lhs=>$lhs, rhs=>$rhs });
      }
    }
    splice(@$rulex, $i, 1, @newrules);
  }
  return $lts;
}

## @newsides = $lts->expand_side(\@side)
##   + returns list of new sides
sub expand_side {
  my ($lts,$side) = @_;
  my $classes = $lts->{classes};
  return $lts->expand_positions([],
				grep {defined($_) && $_ ne '' }
				map {
				  (exists($classes->{$_})
				   ? [sort(keys(%{$classes->{$_}}))]
				   : [$_])
				} @$side);

}

## @expanded = $lts->expand_positions(\@prefix,@suffix_posns)
##  + @posns = [ [pos1_char1, ..., pos1_charN], ..., [posM_char1, ..., posM_charK] ]
sub expand_positions {
  my ($lts,$prefix,@suffix_posns) = @_;
  return ($prefix) if (!@suffix_posns);

  my $sufpos1 = shift(@suffix_posns);
  return map { $lts->expand_positions([@$prefix,$_],@suffix_posns) } @$sufpos1;
}

##==============================================================================
## Methods: Alphabet Expansion
##==============================================================================

## $lts = $lts->expand_alphabet()
##   + instantiates {letters}, {phones}, {specials} keys
sub expand_alphabet {
  my $lts = shift;
  my ($letters,$phones,$specials) = @$lts{qw(letters phones specials)} = ({},{},{});

  ##-- get letters from classes
  @$letters{map { keys(%$_) } values(%{$lts->{classes}})} = undef;

  ##-- get pre-defined phones
  @$phones{keys %{$lts->{phones0}}} = undef;

  ##-- get letters & phones from rules
  my ($r,$part);
  foreach $r (@{$lts->{rules}}) {
    foreach $part (@$r{qw(lhs rhs in)}) {
      @$letters{@$part} = undef;
    }
    @$phones{@{$r->{out}}} = undef;
  }

  ##-- '#' is a special, not anything else
  @$specials{@SPECIALS}=undef;
  delete(@$letters{keys(%$specials), keys(%{$lts->{classes}}) });
  delete(@$phones{keys(%$specials)});

  return $lts;
}


##==============================================================================
## Methods: Ruleset Application
##==============================================================================

##--------------------------------------------------------------
## LTS: Application

## @phones = $lts->apply($word)
## @phones = $lts->apply_word($word)
## @phones = $lts->apply_string($word)
##  + get phones for string $word
*apply = *apply_string = \&apply_word;
sub apply_word {
  my ($lts,$word) = @_;
  return $lts->apply_chars([
			    ($lts->{implicit_bos} ? '#' : qw()),
			    split(//,$word),
			    ($lts->{implicit_eos} ? '#' : qw()),
			   ],
			   ($lts->{implicit_bos} ? 1 : 0));
}

## @phones = $lts->apply_chars($lts,\@word_chars,$initial_position)
##  + get phones for word
*apply_list = \&apply_chars;
sub apply_chars {
  my ($lts,$wchars,$pos) = @_;
  $pos = 0 if (!defined($pos));

  my $rules = $lts->{rules};
  my $classes = $lts->{classes};

  my ($rule,@phones,$newpos);
 CHAR:
  while ($pos <= $#$wchars) {
    if ($wchars->[$pos] eq '#') { ++$pos; next; } ##-- ignore BOS/EOS markers
    foreach $rule (@$rules) {
      if (defined($newpos=$lts->rule_matches($rule,$classes,$wchars,$pos))) {
	if ($lts->{apply_verbose}) {
	  my $vword = join('', @$wchars[0..$pos-1], '_', @$wchars[$pos..$#$wchars]);
	  print STDERR "Match: \'$vword' matches rule $rule->{id}: ", rule2str($rule), "\n";
	}

	push(@phones,@{$rule->{out}});
	$pos = $newpos;
	next CHAR;
      }
    }
    ##-- no match
    if ($lts->{apply_warn}) {
      my $errword = join('', @$wchars[0..$pos-1], '<<HERE>>', @$wchars[$pos..$#$wchars]);
      warn(__PACKAGE__ , ": could not translate word \`$errword' -- skipping");
      last;
    }
    return qw();
  }
  return @phones;
}

## $newpos = $lts->rule_matches($rule,$classes,\@wchars,$pos)
##  + returns new position to start looking in @wchars after matching $rule at $pos,
##    or undef if $rule does not match at $pos
sub rule_matches {
  my ($lts,$rule,$classes,$wchars,$pos) = @_;
  my ($i, $wc, $rc);

  ##-- ensure lengths can match
  return undef if ($pos-@{$rule->{lhs}} < 0 || $pos+@{$rule->{in}}+@{$rule->{rhs}} > @$wchars);

  ##-- match left-hand-side
  for ($i=-1; -$i-1 < @{$rule->{lhs}}; $i--) {
    $wc = $wchars->[$pos+$i];
    $rc = $rule->{lhs}[$i];
    return undef if ($rc ne $wc && (!exists($classes->{$rc}) || !exists($classes->{$rc}{$wc})));
  }

  ##-- match target context
  for ($i=0; $i < @{$rule->{in}}; $i++) {
    $wc = $wchars->[$pos+$i];
    $rc = $rule->{in}[$i];
    return undef
      if ($rc ne $wc);
  }
  $pos += $i;

  ##-- match right-hand-side
  for ($i=0; $i < @{$rule->{rhs}}; $i++) {
    $wc = $wchars->[$pos+$i];
    $rc = $rule->{rhs}[$i];
    return undef if ($rc ne $wc && (!exists($classes->{$rc}) || !exists($classes->{$rc}{$wc})));
 }

  return $pos;
}


##==============================================================================
## Methods: debugging
##==============================================================================

## $str = rule2str($rule)
sub rule2str {
  my $rule = shift;
  return join(' ',
	      '(',
	      @{$rule->{lhs}},
	      '[',
	      @{$rule->{in}},
	      ']',
	      @{$rule->{rhs}},
	      '=',
	      @{$rule->{out}},
	      ')',
	     );
}

## $str = rule2strNS($rule)
##  + no spaces
sub rule2strNS {
  my $rule = shift;
  return join('',
	      '(',
	      @{$rule->{lhs}},
	      '[',
	      @{$rule->{in}},
	      ']',
	      @{$rule->{rhs}},
	      '=',
	      @{$rule->{out}},
	      ')',
	     );
}


##==============================================================================
## Methods: Info
##==============================================================================

# \%info = $lts->info();
sub info {
  my $lts = shift;
  my $info = {};

  $info->{'alph_nLetters'} = scalar(keys(%{$lts->{letters}}));
  $info->{'alph_nPhones'}  = scalar(keys(%{$lts->{phones}}));
  $info->{'alph_nSpecials'} = scalar(keys(%{$lts->{specials}}));
  $info->{'alph_nClasses'} = scalar(keys(%{$lts->{classes}}));

  $info->{'nRules'} = $lts->{rules}  ? scalar(@{$lts->{rules}}) : 0;
  $info->{'nRulesX'} = $lts->{rulex} ? scalar(@{$lts->{rulex}}) : 0;

  my ($min,$max,$r,$len,$key);
  foreach $r (@{$lts->{rules}}) {
    @$len{qw(lhs rhs in out)} = map { scalar(@$_) } @$r{qw(lhs rhs in out)};
    $len->{ltrs} = $len->{lhs} + $len->{in} + $len->{rhs};

    foreach $key (qw(lhs in out rhs ltrs)) {
      $min->{$key} = $len->{$key} if (!defined($min->{$key}) || $len->{$key} < $min->{$key});
      $max->{$key} = $len->{$key} if (!defined($max->{$key}) || $len->{$key} > $max->{$key});
    }
  }
  $info->{"rule_min_len_$_"} = $min->{$_} foreach (keys(%$min));
  $info->{"rule_max_len_$_"} = $max->{$_} foreach (keys(%$max));

  return $info;
}

##==============================================================================
## Methods
##==============================================================================

##==============================================================================
## Generic Utilities
##==============================================================================


## @strings = allstrings($length,\@alphabet)
##   + returns list of all strings of $length from \@alphabet
sub allstrings {
  my ($length,$abet) = @_;
  my @stack = ('',$length);

  my ($prefix,$n);
  my @strings = qw();
  while (($prefix,$n)=splice(@stack,0,2)) {
    if ($n > 0) {
      push(@stack, map { ($prefix.$_,$n-1) } @$abet);
    } else {
      push(@strings,$prefix);
    }
  }
  return @strings;
}

## @lists = alllists($length,\@alphabet)
##   + returns list of all lists of $length from \@alphabet
sub alllists {
  my ($length,$abet) = @_;
  my @stack = ([],$length);

  my ($prefix,$n);
  my @strings = qw();
  while (($prefix,$n)=splice(@stack,0,2)) {
    if ($n > 0) {
      push(@stack, map { ([@$prefix,$_],$n-1) } @$abet);
    } else {
      push(@strings,$prefix);
    }
  }
  return @strings;
}

## @prefixes = allprefixes(\@symbols)
##   + returns list of all non-empty prefixes (array-refs) of \@symbols
sub allprefixes {
  my $syms = shift;
  my ($i);
  my @prefixes = qw();
  for ($i=0; $i < $#$syms; $i++) {
    push(@prefixes, @$syms[0..$i]);
  }
  return (@prefixes,$syms);
}


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
