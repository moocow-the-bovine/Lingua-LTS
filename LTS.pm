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
  return defined($_[1]) ? {%{$_[1]}} : undef;
}


##==============================================================================
## Methods: index generation: flex
##==============================================================================

## undef = $lts->toFlex($file_or_fh,%args);
##  + %args:
##      lacpm=>$lacpm,   ##-- -complete, +expanded (override $lts->{flex_lacpm})
##      ontry=>\&sub,    ##-- called as $sub->($rul) on trial, should return C code string
##      onreject=>\&sub, ##-- called as $sub->($rul) on reject, should return C code string
##      onmatch=>\&sub,  ##-- called as $sub->($rul) on match, should return C code string
##      defs=>$str,      ##-- additional definitions
##      prerules=>$str,  ##-- pre-empting rules
##      postrules=>$str, ##-- post-empting rules (may override default ECHO)
##      code=>$str,      ##-- code (overrides default)
sub toFlex {
  my ($lts,$file, %args) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");

  $fh->print(
	     ##-- Header
	     "/*-*- Mode: Flex -*-*/\n\n",
	     $lts->flexBanner("Header",%args), "\n",
	     $lts->flexHeader(%args),
	     "\n\n",
	     ##-- Definitions
	     $lts->flexBanner("Definitions",%args), "\n",
	     ($args{defs} ? ($args{defs}."\n") : qw()),
	     "\n",
	     $lts->flexDefinitions(%args),
	     "\n",
	     ##-- Rules (LHS)
	     "%%\n",
	     "\n",
	     ($args{prerules} ? ($args{prerules}."\n") : qw()),
	     "\n",
	     $lts->flexRulesLHS(%args),
	     "\n",
	     $lts->flexRulesRHS(%args),
	     "\n",
	     ($args{postrules} ? ($args{postrules}."\n\n") : qw()),
	     "\n",
	     $lts->flexRulesDefault(%args),
	     "\n",
	     ##-- Code
	     "%%\n\n",
	     $lts->flexBanner("Code"), "\n",
	     (exists($args{code}) ? $args{code} : $lts->flexCode(%args)),
	     "\n",
	    );
}

## @bannerlines = $lts->flexBanner($text,%args)
sub flexBanner {
  my ($lts,$text,%args) = @_;
  return (
	  "/*".('-' x 72)."\n",
	  " * $text\n",
	  " */\n",
	 );
}

## @hdrlines = $lts->flexHeader(%args)
##  + requires: $lts->{flex_lacpm} : +reversed, -complete, +expanded
##  + populates: $lts->{flex_lout2id}, $lts->{flex_id2lout}, $lts->{flex_lcs}
##  + %args:
##      lacpm=>$lacpm,   ##-- -complete, +expanded (override $lts->{flex_lacpm})
sub flexHeader {
  my ($lts,%args) = @_;

  ##-- LACPM
  my $lacpm   = $args{lacpm} ? $args{lacpm} : $lts->{flex_lacpm};
  $lacpm = $lts->{flex_lacpm} = $lts->subACPM(which=>[qw(lhs)], reversed=>1, complete=>0)
    if (!$lacpm);

  my $c2lab = $lts->{flex_c2lab} = {"\0"=>0};
  my $lab2c = $lts->{flex_lab2c} = ["\0"];
  foreach (sort(keys(%{$lacpm->{chars}}))) {
    $c2lab->{$_} = scalar(@$lab2c);
    push(@$lab2c, $_);
  }

  ##-- header
  my ($q,%rulids);
  return (
	  "%{\n",
	  "#include <stdio.h>\n",
	  "\n",
	  (
	   ##-- c2lab
	   "static const unsigned int c2lab[".(2**(8*$lacpm->{cw}))."] = {\n",
	   join(",",
		map {
		  (exists($c2lab->{chr($_)}) ? $c2lab->{chr($_)} : -1)
		} (0..(2**(8*$lacpm->{cw})-1))
	       ),
	   " };\n",
	  ),
	  "\n",

	  (##-- goto
	   "static const unsigned int lgoto[$lacpm->{nq}][", (scalar(@$lab2c)), "]",
	   " = {\n",
	   join(",\n",
		map {
		  $q=$_;
		  ("  { "
		   .join(',',
			 map {
			   (defined($lacpm->{goto}[$q]{$lab2c->[$_]})
			    ? $lacpm->{goto}[$q]{$lab2c->[$_]}
			    : -1)
			 } (0..($#$lab2c))
			)
		   ." }")
		} (0..($lacpm->{nq}-1))),
	   " };\n",
	  ),

	  (##-- fail
	   "static const unsigned int lfail[$lacpm->{nq}]",
	   " = { ",
	   join(",", map { $lacpm->{fail}[$_] } (0..($lacpm->{nq}-1))),
	   " };\n",
	  ),
	  "\n",

	  (##-- out
	   "static const unsigned char lout[$lacpm->{nq}][",scalar(@{$lts->{rules}}),"]",
	   " = {\n",
	   (
	    map {
	      $q=$_;
	      %rulids = map {$_=>undef} unpack('S*',$lacpm->{out}{$q});
	      ('{ '
	       .join(',', map { exists($rulids{$_}) ? 1 : 0 } (0..($#{$lts->{rules}})))
	       ." }\n")
	    } (0..($lacpm->{nq}-1))
	   )
	   ." };\n",
	  ),

	  (##-- forward decl
	   "static llookup ... ?!?!?!?",
	  ),

	  "%}",
	 );
}

## @deflines = $lts->flexDefinitions(%args)
sub flexDefinitions {
  my ($lts,%args) = @_;
  ##-- class definitions
  return map { $lts->flexClassDef($_) } sort(keys(%{$lts->{classes}}));
}

##-- @deflines = $lts->flexClassDef($class)
sub flexClassDef {
  my ($lts,$class) = @_;
  my $cname = flexClassName($class);
  return
    (flexClassName($class)
     ."\t["
     .join('', sort keys(%{$lts->{classes}{$class}}))
     ."]\n");
}
sub flexClassDef0 {
  my ($lts,$class) = @_;
  my $cname = flexClassName($class);
  return
    (flexClassName($class)
     ."\t("
     .join('|', map { '"'.$_.'"' } sort keys(%{$lts->{classes}{$class}}))
     .")\n");
}


## @rules_lines = $lts->flexRulesLHS(%args)
sub flexRulesLHS {
  my ($lts,%args) = @_;

  my $lacpm = $lts->{flex_lacpm};
  my $lout2id = $lts->{flex_lout2id};
  my ($lc,$q,$lid);
  return (
	  map {
	    $lc  = $_;
	    $q   = $lacpm->a2q($lc);
	    $lid = $lout2id->{$lacpm->{out}{$q}};
	    ($lts->flexStringLiteral(@$lc)
	     ."\t{ lhsid=$lid;"
	     .($args{onlhs} ? $args{onlhs}->($lc) : '')
	     ." REJECT; }\n")
	  } @{$lts->{flex_lcs}}
	 );
}

## @rhs_rule_lines = $lts->flexRulesRHS(%args)
sub flexRulesRHS {
  my ($lts,%args) = @_;
  my ($rul);
  return (
	  map {
	    $rul=$_;
	    (''
	     .$lts->flexStringLiteral(@{$rul->{in}})
	     .(@{$rul->{rhs}}
	       ? ("/".$lts->flexString(@{$rul->{rhs}}))
	       : '')
	     ."\t{ "#."\n\t
	     .($args{ontry} ? $args{ontry}->($rul) : '')
	     ." if (!lid2rulid[lhsid][$rul->{id}]) {"
	     .($args{onreject} ? $args{onreject}->($rul) : '')
	     .' REJECT; }'
	     ."\t "
	     .($args{onmatch}
	       ? $args{onmatch}->($rul)
	       : ('fputs("'.join(' ', @{$rul->{out}}).' ", stdout);'))
	     ." }\n"
	     .'')
	  } (@{$lts->{rules}})
	 );
}

## @default_rule_lines = $lts->flexRulesDefault(%args)
sub flexRulesDefault {
  my ($lts,%args) = @_;

  return (
	  ##-- DUMMY
	  #".\t{ ECHO; printf(\"(%d)\", lhsid); lhsid=0; }\n",
	  #".\t{ lhsid=0; }\n",
	  ".\t{ ECHO; }\n",
	 );
}

## @code_lines = $lts->flexCode(%args)
sub flexCode {
  my ($lts,%args) = @_;
  return (
	  "int main (void) { yylex(); return 0; }\n",
	  "\n",
	  "int yywrap (void) { return 1; }\n",
	 );
}


##-- flex-safe class-name
## $name = flexClassName($classname);
sub flexClassName {
    my $class =shift;
    if    ($class eq '*') { return '_STAR_'; }
    elsif ($class eq '+') { return '_PLUS_'; }
    return $class;
}

##-- flex-safe symbol string
## $string = $lts->flexString(@symbols)
sub flexString {
  my ($lts,@syms) = @_;
  return '""' if (!@syms);
  return join('',
	      map {
		(exists($lts->{classes}{$_})
		 ? ('{'.flexClassName($_).'}')
		 : ('"'.$_.'"'))
	      } @syms);
}

##-- flex-safe symbol string
## $string = $lts->flexStringLiteral(@symbols)
sub flexStringLiteral {
  my ($lts,@syms) = @_;
  return '"'.join('',@syms).'"';
}

##==============================================================================
## Methods: index generation: dual-ACPM Gfsm
##==============================================================================

##-- looks correct, too expensive

##--------------------------------------------------------------
## Methods: index generation: Gfsm: Labels

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

##--------------------------------------------------------------
## Methods: index generation: Gfsm: FST

## $gfsmFST             = $lts->gfsmFST(%args)
## ($fst,$ilabs,$olabs) = $lts->gfsmFST(%args)
##  + requires: $lts->{acpm} (NOT complete)
##  + populates keys:
##     fst=>$fst,
##     fstilabs=>$ilabs,
##     fstolabs=>$olabs,
##  + %args:
##     ilabels=>$ilabs, ##-- default = $lts->gfsmLabels()
##     olabels=>$olabs, ##-- default = $ilabs
##     verbose=>$bool,  ##-- report verbosely
*gfsmFST = \&gfsmTransducer;
sub gfsmTransducer {
  my ($lts,%args) = @_;

  ##-------------------------
  ## LHS

  ##-- LHS: generate ACPM
  print STDERR (ref($lts), "::gfsmTransducer(): generating left-context ACPM... ") if ($args{verbose});
  my $lacpm = $lts->subacpm(which=>[qw(lhs)]);
  print STDERR ("done.\n") if ($args{verbose});

  ##-- LHS: build & adjust lacpm fst
  print STDERR (ref($lts), "::gfsmTransducer(): building left-context FST... ") if ($args{verbose});

  ##-- LHS: FST: labels
  my $lilabs   = $lacpm->gfsmInputLabels();
  my $lilabs_a = $lilabs->asArray();
  my $lolabs = $lacpm->gfsmOutputLabels();
  my $nlolabs = $lolabs->size;
  my @lisyms = grep { $_ ne '<eps>' && $_ ne '<fail>' } @$lilabs_a;
  $lolabs->insert($_) foreach (@lisyms);
  my $lolabs_h = $lolabs->asHash;
  my $lolabs_a = $lolabs->asArray;

  my $lfst = $lacpm->gfsmTransducer(ilabels=>$lilabs,olabels=>$lolabs,dosort=>0);
  my $qmax = $lfst->n_states();
  my $ai = Gfsm::ArcIter->new();
  my ($q,$r);
  foreach $q (0..($lacpm->{nq}-1)) {
    ##-- replace arcs ($q --a:OutL($q)--> $qto) with ($q --a:OutL($q)--> $r=newState() --eps:a--> $qto)
    for ($ai->open($lfst,$q); $ai->ok; $ai->next) {
      $r = $qmax++;
      $lfst->add_arc($r,$ai->target, 0,$lolabs_h->{$lilabs_a->[$ai->lower]}, 0);
      $ai->upper($lolabs_h->{$lacpm->{out}{$q}});
      $ai->target($r);
    }
  }
  print STDERR ("done.\n") if ($args{verbose});


  ##-------------------------
  ## IN.RHS (reversed)

  ##-- IN.RHS: generate ACPM
  print STDERR (ref($lts), "::gfsmTransducer(): generating right-context ACPM... ") if ($args{verbose});
  my $racpm = $lts->subacpm(which=>[qw(in rhs)], reversed=>1);
  print STDERR ("done.\n") if ($args{verbose});

  ##-- IN.RHS: build & adjust racpm fst
  print STDERR (ref($lts), "::gfsmTransducer(): building right-context FST... ") if ($args{verbose});

  ##-- IN.RHS: FST: labels
  my $rilabs   = $lolabs;
  my $rilabs_a = $rilabs->asArray;
  my $rilabs_h = $rilabs->asHash;

  my $rolabs   = $racpm->gfsmOutputLabels();
  my $nrolabs  = $rolabs->size;
  $rolabs->insert($_) foreach (@{$lolabs->asArray}[1..($nlolabs-1)]);
  my $rolabs_h = $rolabs->asHash;

  #my $colabs_d = Gfsm::Alphabet->new();
  #$colabs_d->insert('<eps>',0);
  #$colabs_d->insert('<none>',1);
  #$colabs_d->insert(rule2str($lts->{rules}[$_]), $_+2) foreach (0..$#{$lts->{rules}});

  ##-- IN.RHS: map ($packed_rulid == $rulid+1) => \@lolabs_containing_rulid
  my $outid2lolabs = [[]];
  my ($loseti,$prulid);
  foreach $loseti (1..($nlolabs-1)) {
    foreach $prulid (unpack('S*', $lolabs_a->[$loseti])) {
      push(@{$outid2lolabs->[$prulid]}, $loseti);
    }
  }

  ##-- RHS: build & adjust racpm fst
  my $rfst = $racpm->gfsmTransducer(ilabels=>$rilabs,olabels=>$rolabs,dosort=>0);
  $qmax = $rfst->n_states();
  my (%rdeltaq, $qto, $routid,$lolabids,%usedlolabs,$lolab);
  foreach $q (0..($racpm->{nq}-1)) {
    print STDERR "." if ($args{verbose} && $q % 512 == 0);

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
	$qmax++;

	##-- get best output for each left-context set outL \in out(Q_L), outR($qto)
	##   + add arcs: ($r --OutL:OutBest(OutL,$qto)--> $qto)
	if ($racpm->{out}{$qto}) {
	  %usedlolabs = qw();
	  foreach $routid (unpack('S*', $racpm->{out}{$qto})) {
	    $lolabids = $outid2lolabs->[$routid];
	    foreach $lolab (grep {!exists($usedlolabs{$_})} (defined($lolabids) ? @$lolabids : qw())) {
	      $rfst->add_arc($r,$qto, $lolab, $routid+1, 0);
	      $usedlolabs{$lolab}=undef;
	    }
	  }
	} else {
	  ##-- HACK
	  $rfst->add_arc($r,$qto, 1,1, 0);
	}
      }
    }
  }
  print STDERR ("done.\n") if ($args{verbose});

  ##-------------------------
  ## Ouput filter

  print STDERR (ref($lts), "::gfsmTransducer(): building output filter... ") if ($args{verbose});

  ##-- output filter: labels
  my $folabs = Gfsm::Alphabet->new;
  $folabs->insert('<eps>',0);
  $folabs->insert($_) foreach (sort(keys(%{$lts->{phones}})));

  ##-- output filter: fst
  my $filter = Gfsm::Automaton->new;
  $filter->is_transducer(1);
  $filter->is_weighted(0);
  $filter->root(0);
  $filter->is_final(0,1);
  $filter->add_arc(0,0, 1,0, 0);
  my @consume = (0,0);
  my ($rul,$rulid,$inlen,$rulout,$qfrom,$clen,$colab,$i);
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
	foreach $colab (1..($#{$lts->{rules}}+2)) {
	  $filter->add_arc($qfrom, $qto, $colab,0, 0);
	}
      }
    }

    ##-- build output path (0 -- $rulid+2 : OUT($rul) --> $consume[$inlen])
    $rulout = $rul->{out};
    $filter->add_arc(0,        ($#$rulout <= 0 ? $consume[$inlen]                 : ($qmax++)),
		     $rulid+2, ($#$rulout >= 0 ? $folabs->get_label($rulout->[0]) : 0),
		     0);
    foreach $i (1..$#$rulout) {
      $filter->add_arc($qmax-1, ($i==$#$rulout ? $consume[$inlen] : ($qmax++)),
		       0,       $folabs->get_label($rulout->[$i]),
		       0);
    }
  }
  print STDERR ("done.\n") if ($args{verbose});

  return ($lfst,$rfst,$filter,$lilabs,$folabs); ##-- DEBUG

  ##-------------------------
  ## Compose: LHS ° RHS
  print STDERR (ref($lts), "::gfsmTransducer(): composing transducers... ") if ($args{verbose});
  $lfst->arcsort(Gfsm::ASMUpper());
  $rfst->_reverse;
  $rfst->arcsort(Gfsm::ASMLower());

  my $cfst = $lfst->compose($rfst);
  $cfst->arcsort(Gfsm::ASMUpper());
  $cfst->_compose($filter);
  $cfst->_connect;
  $cfst->renumber_states;

  print STDERR "done.\n" if ($args{verbose});

  @$lts{qw(fst fstilabs fstolabs)} = ($cfst,$lilabs,$folabs);

  return wantarray ? ($cfst,$lilabs,$folabs) : $cfst;
}

##--------------------------------------------------------------
## Methods: index generation: Gfsm: partial Trie

## $acpm = subTrie($lts,%args)
## + %args:
##    reversed=>$bool,
##    which=>\@rule_keys,
##    rules=>\@rules,   ##-- default: $lts->{rulex}
*subtrie = \&subTrie;
sub subTrie {
  my ($lts,%args) = @_;

  my $trie = Lingua::LTS::Trie->new();
  @{$trie->{chars}}{ (
		      keys(%{$lts->{letters}}),
		      keys(%{$lts->{specials}})
		     )
		   } = undef;

  my $rules = (defined($args{rules}) ? $args{rules} : $lts->{rulex});
  my ($q,$r,@rsyms);
  foreach $r (@$rules) {
    @rsyms = map { @$_ } @$r{@{$args{which}}};
    if (defined($q=$trie->a2q(\@rsyms))) {
      $trie->{out}{$q}{$r->{id}}=undef;
    } else {
      $trie->addArray(\@rsyms,{$r->{id}=>undef});
    }
  }
  return $trie;
}


##--------------------------------------------------------------
## Methods: index generation: Gfsm: partial ACPM

## $acpm = subACPM($lts,%args)
## + %args:
##    reversed=>$bool,
##    which=>\@rule_keys,
##    rules=>\@rules,   ##-- default: $lts->{rulex}
##    complete=>$bool,  ##-- defulat: true
*subacpm = \&subACPM;
sub subACPM {
  my ($lts,%args) = @_;
  my $trie = $lts->subTrie(%args);
  my $acpm = Lingua::LTS::ACPM->newFromTrie($trie,
					    joinout=>\&_acpm_joinout
					   );

  ##-- pack output function
  my ($q);
  foreach $q (0..($acpm->{nq}-1)) {
    $acpm->{out}{$q} = pack('S*',
			    (defined($acpm->{out}{$q})
			     ? (map { $_+1 } sort { $a <=> $b } keys(%{$acpm->{out}{$q}}))
			     : qw()));
  }

  $acpm->complete() if ($args{complete} || !defined($args{complete}));
  return $acpm;
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
