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
our @SPECIALS = ('=<epsilon>', '#');
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

  ##-- print letters [history]
  $lts->print_symbols_lines($fh,'History',[map { "-$_" } sort(keys(%{$lts->{letters}}))]);
  $fh->print("\n");

  ##-- print phones (temporary)
  $lts->print_symbols_lines($fh,'Phon',   [map { "=$_" } sort(keys(%{$lts->{phones}}))]);
  $fh->print("\n");

  ##-- print phones (output)
  $lts->print_symbols_lines($fh,'PhonOut',[              sort(keys(%{$lts->{phones}}))]);
  $fh->print("\n");

  ##-- print classes
  my $classes = $lts->{classes};
  my ($c);
  foreach $c (sort(keys(%$classes))) {
    $lts->print_symbols_lines($fh,"Class$c", [              sort(keys(%{$classes->{$c}}))]);
    $fh->print("\n");
    $lts->print_symbols_lines($fh,"-Class$c",[map { "-$_" } sort(keys(%{$classes->{$c}}))]);
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



##--------------------------------------------------------------
## Methods: I/O: Output: AT&T context-sensitive rewrite rules

## undef = $lts->to_csrules($filename_or_fh)

##------ STILL BUGGY!

##------ PROBLEM (solved, old):
##ex LTS:
##
## a [ a ] = B
##   [ a ] = A
##
##--> rules
##
## a -> B / a __
## a -> A / __
##
##--> BUT:
##
## lookup('a')   => 'A'   # ok
## lookup('aa')  => 'AB'  # ok
## lookup('aaa') => 'ABA' # NOT ok
##
## SOLUTION (?): add 'simultaneous' keyword!

sub to_csrules {
  my ($lts,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak(__PACKAGE__, "::to_csrules(): open failed for '$file': $!") if (!$fh);

  ##-- print lexrulecomp compiler options
  $fh->print("simultaneous\n", "obligatory\n", "\n");

  my $rules = $lts->{rules};
  my ($r,$ii);
  my $i = 1;

  foreach $ii (
	      sort {
		(@{$rules->[$b]{in}} <=> @{$rules->[$a]{in}}      ##-- length(r->IN), descending
		 || @{$rules->[$b]{lhs}} <=> @{$rules->[$a]{lhs}} ##-- length(r->LHS), descending
		 || @{$rules->[$b]{rhs}} <=> @{$rules->[$a]{rhs}} ##-- length(r->RHS), descending
		 || $a <=> $b)                                    ##-- index(r->INDEX), ascending
	      } (0..$#$rules)
	     )
    {
      $r = $rules->[$ii];
      $lts->print_csrule_line_x($fh, @$r{qw(lhs in out rhs)}, "RULE ".($i++)." [$ii]: ".rule2strNS($r));
    }

  $fh->close if (!ref($file));
  return $lts;
}

## undef = print_csrule_line_x($fh,\@left,\@in,\@out,\@right,@comments)
sub print_csrule_line_x {
  my ($lts,$fh,$lc,$in,$out,$rc,@comments) = @_;
  my ($lenPhi,$lenPsi,$lenL,$lenR) = (8,16,16,16);
  $fh->print(
	     sprintf("%${lenPhi}s -> %-${lenPsi}s / %${lenL}s __ %-${lenR}s %s\n",
	     #sprintf("%s -> %s / %s __ %s   %s\n",
		     ##-- input (target: Phi)
		     join('', map { $lts->att_str($_) } @$in),

		     ##-- output (target: Psi)
		     join('',
			  ##-- output: history
			  (map { $lts->att_str($_,'-') } @$in),
			  ##-- output: phones
			  (map { $lts->att_str($_,'=') } @$out),
			 ),

		     ##-- left context (history)
		     join('',
			  map { ($lts->att_str((exists($lts->{classes}{$_}) ? "Class$_" : $_),
					       '-'), ##-- history
				 '([Phon]*)')
			       } @$lc),

		     ##-- right context
		     join('',
			  map { ($lts->att_str((exists($lts->{classes}{$_}) ? "Class$_" : $_),
					       ''), ##-- no history
				 '([Phon]*)')
			       } @$rc),

		     ##-- comments
		     (@comments ? join('','# ', @comments) : ''),
		    )
	    );
}


## $str = $lts->att_str($symbol)
## $str = $lts->att_str($symbol,$prefix)
##  + returns AT&T-safe symbol string (with prefix "$prefix"),
##    honoring no-prefix convention for specials
sub att_str {
  my ($lts,$str,$prefix) = @_;
  return !$prefix || exists($lts->{specials}{$str}) ? _att_safe_str($str) : "[${prefix}${str}]";
}

## $str = _att_safe_str($symbol)
##  + lex-safe symbol
sub _att_safe_str {
  my $sym = shift;
  return "[<epsilon>]" if (length($sym)==0);
  return "[$sym]"      if (length($sym) > 1);
  return ("\\".$sym)   if ($sym =~ /[\#\+\*\-\^\@\:\[\]]/);
  return $sym;
}

##==============================================================================
## Methods: index generation
##==============================================================================

## $automaton = $lts->toAutomaton()
## $automaton = $lts->toAutomaton($automaton)
##   + requires: expand_alphabet(), expand_rules()
##   + INCOMPLETE
sub toAutomaton {
  my ($lts,$fst) = @_;
  $fst = Lingua::LTS::Automaton->new() if (!$fst);
  @$fst{qw(specials letters phones)} = @$lts{qw(specials letters phones)};

  ##-- get all input symbols
  my @ltrs = ('#', keys(%{$lts->{letters}}));

  ##-- generate automaton: step1: trie
  my $rulex = $lts->{rulex};
  #my $qid2dot = {};
  my $qid;
  my ($r);
  foreach $r (@$rulex) {
    #$qid = $fst->addString(join('', @{$r->{lhs}}, @{$r->{in}}, @{$r->{rhs}}));
    #$qid2dot->{$qid} = @{$r->{lhs}} + @{$r->{in}};
    $fst->addRule(@$r{qw(lhs in out rhs)});
  }

  ##-- @queue: potential contexts to be investigated:
  ##   @queue = ( $potential_context_1, ..., $potential_context_n )
  #my @queue = ($lts->{implicit_bos} ? '#' : '');
  #my ($ctx);
  #while (defined($ctx=shift(@queue))) { ???; }

  return $fst;
}

##==============================================================================
## Methods: index generation: ACPM
##==============================================================================

## $acpm = $lts->toACPM(%args)
##  + %args:
##     complete=>$bool  # complete the ACPM
##  + requires: expand_rules(), expand_alphabet()
##  + populates: $lts->{acpm}
*compile_acpm = *toACPM = \&toACPM_0;
sub toACPM_0 {
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

  ##-- step 2: generate ACPM
  my $acpm = $lts->{acpm} = Lingua::LTS::ACPM->newFromTrie($trie,joinout=>\&_acpm_joinout_0);
  $acpm->complete() if ($args{complete});

  return $acpm;
}

## \%idhash = _acpm_joinout_0($hash1_or_undef, $hash2_or_undef)
sub _acpm_joinout_0 {
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
sub gfsmSymbols {
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
sub gfsmTransducer {
  my ($lts,%args) = @_;
  my $ilabs = $args{ilabels} ? $args{ilabels} : $lts->gfsmLabels();
  my $olabs = $args{olabels} ? $args{olabels} : $ilabs;
  my $acpm  = $lts->{acpm};
  my $goto  = $acpm->{goto};
  my $rgoto = $acpm->{rgoto};
  my $fst   = $acpm->gfsmAutomaton(ilabels=>$ilabs,dosort=>0);

  ##-- Init: build reverse-arc index for ACPM arcs
  #my @Q = (0..($acpm->{nq}-1));
  #my $rdelta = []; ##-- $qto=>{$a=>$qfrom,...}
  #my ($q,$a,$qto);
  #foreach $q (@Q) {
  #  while (($a,$qto)=each(%{$goto->[$q]})) {                   ##-- reverse arc-index
  #    $rdelta->[$qto]{$a} = $q;
  #  }
  #}

  ##-- Phase 1:
  ##    + map output rules to the states at which they would apply,
  ##      factoring out input and right-hand side

  ##-- ##$q=>{ "${rulid}:${n_read}/${n_lhs},${n_lhs_in},${n_lhs_in_rhs}" }
  ##-- $q=>{ pack('LS4', ${rulid}, ${n_read}, ${n_lhs} ${n_lhs_in} ${n_lhs_in_rhs}), ... }
  my $q2rulpos = $lts->{q2rulpos} = [];
  my $rules = $lts->{rules};
  my ($q,$qout,$rulid,$rul, $lenL,$lenLI,$lenLIR,$nread,$r);
  while (($q,$qout)=each(%{$acpm->{out}})) {
    foreach $rulid (keys %$qout) {
      $rul    = $rules->[$rulid];
      $lenL   = @{$rul->{lhs}};
      $lenLI  = $lenL + @{$rul->{in}};
      $lenLIR = $lenLI + @{$rul->{rhs}};

      ##-- back up
      for ($nread=$lenLIR, $r=$q; $nread > 0 && defined($r); $nread--) {
	$q2rulpos->[$r]{pack('LS4', $rulid,$nread,$lenL,$lenLI,$lenLIR)} = undef;
	$r = (split(/ /,$rgoto->[$r]))[0];
      }
    }
  }

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
## Methods: index generation: Gfsm
##==============================================================================

## undef = $lts->compile_tries()
##  + requires: expand_rules(), expand_alphabet()
##  + populates keys:
##
##     labs => $gfsmAlphabet,
##     key2lab => $alphabetHash,
##     lab2key => $alphabetArray,
##
##     lsta => $left_context_sta,
##     rpta => $input_right_context_pta,
##
##     lsta2rules => { $qid_lsta=>pack('L*', sort{$a<=>$b} @rule_indices), ... }
##     rpta2rules => { $qid_lsta=>pack('L*', sort{$a<=>$b} @rule_indices), ... }
#use Gfsm;
sub compile_tries0 {
  my $lts = shift;
  my $rulex = $lts->{rulex};
  my $labs = $lts->{labs} = Gfsm::Alphabet->new;
  my $lsta = $lts->{lsta} = Gfsm::Automaton->newTrie;
  my $rpta = $lts->{rpta} = Gfsm::Automaton->newTrie;

  ##-- populate labels
  $labs->insert($_) foreach ('<epsilon>', '#',
			     sort(keys(%{$lts->{letters}})),
			     sort(keys(%{$lts->{phones}})),
			    );
  my $key2lab = $lts->{key2lab} = $labs->asHash;
  my $lab2key = $lts->{lab2key} = $labs->asArray;

  ##-- populate context PTAs
  my ($ri,$xi,$r,$qids);

  ##-- partial rules
  my $lsta2partial = $lts->{lsta2partial} = [];
  my $rpta2partial = $lts->{rpta2partial} = [];

  ##-- full rules
  my $lsta2full = $lts->{lsta2full} = {};
  my $rpta2full = $lts->{rpta2full} = {};

  foreach $xi (0..$#$rulex) {
    $r  = $rulex->[$xi];
    $ri = $r->{id};

    ##-- left context
    $qids = $lsta->add_path_states([reverse(@$key2lab{@{$r->{lhs}}})], [], 0.0, 0,0,1);
    $lsta2partial->[$_] .= pack('L',$ri) foreach (@$qids);
    $lsta2full->{$qids->[$#$qids]} .= pack('L',$ri);

    ##-- right context
    $qids = $rpta->add_path_states([@$key2lab{@{$r->{in}},@{$r->{rhs}}}], [], 0.0, 0,0,1);
    $rpta2partial->[$_] .= pack('L',$ri) foreach (@$qids);
    $rpta2full->{$qids->[$#$qids]} .= pack('L',$ri);
  }

  ##-- cleanup maps
  my ($q,%ids);
  foreach $q (0..$#$lsta2partial) {
    next if (!defined($lsta2partial->[$q]));

    %ids = map {$_=>undef} unpack('L*',$lsta2partial->[$q]);
    $lsta2partial->[$q] = pack('L*', sort {$a<=>$b} keys %ids);

    if (defined($lsta2full->{$q})) {
      %ids = map {$_=>undef} unpack('L*',$lsta2full->{$q});
      $lsta2full->{$q} = pack('L*', sort {$a<=>$b} keys %ids);
    }
  }
  foreach $q (0..$#$rpta2partial) {
    next if (!defined($rpta2partial->[$q]));

    %ids = map {$_=>undef} unpack('L*',$rpta2partial->[$q]);
    $rpta2partial->[$q] = pack('L*', sort {$a<=>$b} keys %ids);

    if (defined($rpta2full->{$q})) {
      %ids = map {$_=>undef} unpack('L*',$rpta2full->{$q});
      $rpta2full->{$q} = pack('L*', sort {$a<=>$b} keys %ids);
    }
  }


  ##-- sort index tries
  $lsta->arcsort(Gfsm::ASMLower());
  $rpta->arcsort(Gfsm::ASMLower());
}

##--------------------------------------------------------------
## LTS: Indexed Application

## @phones = $lts->apply_indexed($word)
##  + get phones for string $word
##  + requires: compile_tries()

#*apply_indexed = *apply_word_indexed = \&apply_word_indexed_gfsm;
sub apply_word_indexed_gfsm {
  my ($lts,$word) = @_;
  return $lts->apply_chars_indexed([
				    ($lts->{implicit_bos} ? '#' : qw()),
				    split(//,$word),
				    ($lts->{implicit_eos} ? '#' : qw()),
				   ],
				   ($lts->{implicit_bos} ? 1 : 0));
}

## @phones = $lts->apply_chars_indexed($lts,\@word_chars,$initial_position)
##  + get phones for word
##  + requires: compile_tries()
sub apply_chars_indexed_gfsm {
  my ($lts,$wchars,$pos) = @_;
  $pos = 0 if (!defined($pos));
  my @wlabs = map {defined($_) ? $_ : $Gfsm::noLabel} @{$lts->{key2lab}}{@$wchars};

  my (@phones, $qids_l,$lo_i_l, $qids_r,$lo_i_r, %ruleids,%ruleids_l, $ruli,$rul);
 CHAR:
  while ($pos <= $#$wchars) {
    if ($wchars->[$pos] eq '#') { ++$pos; next; } ##-- ignore BOS/EOS markers

    ##-- get matching states
    ($qids_r,$lo_i_r) = $lts->{rpta}->find_prefix_states([@wlabs[$pos..$#wlabs]],[]);
    %ruleids   = map {$_=>undef} map {defined($_) ? unpack('L*', $_) : qw()} @{$lts->{rpta2full}}{@$qids_r};

    ($qids_l,$lo_i_l) = $lts->{lsta}->find_prefix_states([reverse(@wlabs[0..($pos-1)])],[]);
    %ruleids_l = map {$_=>undef} map {defined($_) ? unpack('L*', $_) : qw()} @{$lts->{lsta2full}}{@$qids_l};

    delete(@ruleids{grep {!exists($ruleids_l{$_})} keys %ruleids});

    $ruli=(sort {$a<=>$b} keys(%ruleids))[0];

    if (!defined($ruli)) {
      if ($lts->{apply_warn}) {
	my $errword = join('', @$wchars[0..($pos-1)], '<<HERE>>', @$wchars[$pos..$#$wchars]);
	warn(__PACKAGE__ , ": could not translate word \`$errword' -- skipping");
	last;
      }
      return qw();
    }

    $rul = $lts->{rules}[$ruli];
    if ($lts->{apply_verbose}) {
      my $vword = join('', @$wchars[0..($pos-1)], '_', @$wchars[$pos..$#$wchars]);
      print STDERR "Match: \'$vword\' matches rule $rul->{id}: ", rule2str($rul), "\n";
    }

    push(@phones,@{$rul->{out}});
    $pos += @{$rul->{in}};
    next CHAR;
  }
  return @phones;
}

##==============================================================================
## Methods: index generation: brute force
##==============================================================================


##-- brute force search
##   + requires: expand_alphabet()
##   + INCOMPLETE
sub bruteForceSearch {
  my $lts = shift;

  my @fifo = ( {left=>'',right=>'',out=>'',rules=>[0..$#{$lts->{rulex}}], } );
  ##-- fifo: ( $h1, ..., )
  ##  $h = {
  ##        left  => $left_ctx,
  ##        right => $in . $right_ctx,
  ##        out   => $buf_output,
  ##        rules => \@potential_match_indices,
  ##       }
  my $hists = $lts->{bruteHistories} = {};
  my ($h,$hstr, $ruli,$rul, $h2,$h2r, $nextL,$nextR);
  while (defined($h=shift(@fifo))) {
    $hstr = "$h->{left}.$h->{right}:$h->{out}";
    next if (exists($hists->{$hstr})); ##-- avoid loops
    $hists->{$hstr} = undef;           ##-- mark history as visited

    if (@{$h->{rules}}==0) {
      ##-- no potential matches: too much history

      ##-- what to do here?!
      next;
    }
    if (@{$h->{rules}}==1) {
      ##-- only 1 rule matches: flush output & continue
      $ruli = $h->{rules}[0];
      $rul  = $lts->{rulex}[$ruli];
      $h2r  = $h->{right};
      substr($h2r,0,@{$rul->{in}},'');
      $h2  = { left=>$h->{left}.$rul->{in}, right=>$h2r, out=>'', };
      $h2->{rules} = $lts->potentialMatches($h2);
      push(@fifo,$h2);
      next;
    }
    ##-- multiple rules may match: extend histories
    push(@fifo,@{$lts->extendLeft($h)});
    push(@fifo,@{$lts->extendRight($h)});
  }

  return $lts;
}

## TODO: \@potentially_matching_rulex_indices = $lts->potentialMatches($history)
sub potentialMatches {
  my ($lts,$h) = @_;

  ##-- find all potentially matching rule indices
  my (%indices_r,%indices_l) = qw();

  ##-- left context
  my ($qid_l,$lo_i_l) = $lts->{lsta}->find_prefix([reverse(@{$lts->{key2lab}}{split(//,$h->{left})})], []);
  return [] if ($lo_i_l != length($h->{left}));

  ##-- right context
  my ($qid_r,$lo_i_r) = $lts->{rpta}->find_prefix([@{$lts->{key2lab}}{split(//,$h->{right})}], []);
  return [] if ($lo_i_r != length($h->{right}));

  ##-- index hashes
  @indices_l{unpack('L*',$lts->{lsta2rules}[$qid_l])} = undef;
  @indices_r{unpack('L*',$lts->{rpta2rules}[$qid_r])} = undef;

  ##-- joint: left & right
  delete(@indices_r{grep {!exists($indices_l{$_})} keys(%indices_r)});

  #return [map { $lts->{rulex}[$_] } sort {$a<=>$b} keys(%indices_r)];
  return [sort {$a<=>$b} keys(%indices_r)];
}

## TODO: $h2 = $lts->extendLeft($h)
sub extendLeft {
  my ($lts,$h) = @_;

  my ($qid,$lo_i) = $lts->{lsta}->find_prefix([reverse(@{$lts->{key2lab}}{split(//,$h->{left})})], []);
  return [] if ($lo_i != length($h->{left}));

  ##-- single-letter extensions
  my @extensions = qw();
  my ($ai,%h2rules_l,$h2rules);
  for ($ai=Gfsm::ArcIter->new($lts->{lsta},$qid); $ai->ok; $ai->next()) {
    %h2rules_l = map { $_=>undef } unpack('L*',$lts->{lsta2rules}[$ai->target]);
    $h2rules = [grep {exists($h2rules_l{$_})} @{$h->{rules}}];
    push(@extensions, {
		       left =>($lts->{lab2key}[$ai->lower] . $h->{left}),
		       right=>$h->{right},
		       out  =>$h->{out},
		       rules=>$h2rules,
		      });
  }

  return \@extensions;
}

## TODO: $h2 = $lts->extendRight($h,$letter)
sub extendRight {
  my ($lts,$h) = @_;

  my ($qid,$lo_i) = $lts->{rpta}->find_prefix([@{$lts->{key2lab}}{split(//,$h->{left})}], []);
  return [] if ($lo_i != length($h->{right}));

  ##-- single-letter extensions
  my @extensions = qw();
  my ($ai,%h2rules_l,$h2rules);
  for ($ai=Gfsm::ArcIter->new($lts->{rpta},$qid); $ai->ok; $ai->next()) {
    %h2rules_l = map { $_=>undef } unpack('L*',$lts->{rpta2rules}[$ai->target]);
    $h2rules = [grep {exists($h2rules_l{$_})} @{$h->{rules}}];
    push(@extensions, {
		       left =>$h->{left},
		       right=>($h->{right} . $lts->{lab2key}[$ai->lower]),
		       out  =>$h->{out},
		       rules=>$h2rules,
		      });
  }

  return \@extensions;
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
