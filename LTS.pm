## -*- Mode: CPerl -*-
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

our %VERBOSE = (
		silent=>0,
		error=>1,
		warn=>2,
		info=>3,
		progress=>4,
		debug=>255,
		default=>255,
	       );

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
##     keep     => \%keepers,     ##-- pseudo-set: req: expand_alphabet() [for fst generation]
##     ##
##     ##-- Options
##     implicit_bos => $bool,     ##-- default=1
##     implicit_eos => $bool,     ##-- default=1
##     ##
##     ##-- debugging
##     apply_verbose => $bool,
##     apply_warn=>$bool,
##     verbose=>$level,           ##-- for operations which might take a long time
##
sub new {
  my $that = shift;
  return bless({
		classes=>{},
		rules=>[],

		##-- expanded alphabet
		letters=>{},
		phones=>{},
		specials=>{%SPECIALS},

		##-- loaded alphabet
		phones0=>{},
		keep0=>{},
		specials0=>{},

		##-- flags
		apply_verbose=>0,
		apply_warn=>1,
		implicit_bos=>1,
		implicit_eos=>1,
		verbose=>$VERBOSE{default},
		@_
	       }, ref($that)||$that);
}

##==============================================================================
## Methods: messages
##==============================================================================

## undef = $lts->vmsg($minLevel, @msg)
##  + $minLevel is either an int (literal) or a symbolic name
sub vmsg {
  $_[0]->vmsg0($_[1], ref($_[0]), ': ', @_[2..$#_]);
}

## undef = $lts->vmsg($minLevel, @msg)
##  + $minLevel is either an int (literal) or a symbolic name
##  + no prefix is printed
sub vmsg0 {
  my ($lts,$minLevel,@msg) = @_;
  $minLevel = $VERBOSE{$minLevel} if (defined($VERBOSE{$minLevel}));
  $lts->{verbose} = $VERBOSE{default} if (!defined($lts->{verbose}));
  return if ($minLevel > $lts->{verbose});
  print STDERR @msg;
}

##==============================================================================
## Methods: I/O
##==============================================================================

##--------------------------------------------------------------
## Methods: I/O: Input: .lts

## $obj = $CLASS_OR_OBJ->load($filename_or_fh)
##  + File Syntax:
##    LTS_FILE ::= LTS_LINE*
##    LTS_LINE ::= ( BLANK | COMMENT | PHON | SPECIAL | KEEP | CLASS | IGNORE | RULE ) "\n"
##    BLANK    ::= (whitespace)
##    COMMENT  ::= ";" (anything)
##    PHON     ::= "phon" PHONSYMS
##    SPECIAL  ::= "special" SPECIALSYMS
##    KEEP     ::= "keep" KEEPSYMS
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

  my ($cname,@cchars, $lhs,$in,$rhs,$out);
  while (<$fh>) {
    chomp;
    s/(?<!\\)\;.*//g;   ##-- ignore comments
    next if (/^\s*$/);  ##-- ... and blank lines
    s/\\(.)/$1/g;       ##-- un-escape

    if (/^\s*special\s+(.*\S)\s*/) {
      @cchars = split(/\s+/,$1);
      @{$lts->{special0}}{@cchars} = undef;
    }
    elsif (/^\s*keep\s+(.*\S)\s*/) {
      @cchars = split(/\s+/,$1);
      @{$lts->{keep0}}{@cchars} = undef;
    }
    elsif (/^\s*phon\s+(.*\S)\s*/) {
      @cchars = split(/\s+/,$1);
      @{$lts->{phones0}}{@cchars} = undef;
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
## Methods: I/O: Input: .sym

## $obj = $CLASS_OR_OBJ->load_symbols($filename_or_fh,%args)
##  + load AT&T-format symbols file
##  + populates keys: symLetters, symPhones, symSpecial, symLines
##  + %args
##     Letter  => \@letterClassNames,  ##-- default: ['LtsLetter']
##     Phon    => \@phonClassNames,    ##-- default: ['LtsPhon']
##     Special => \@specialClassNames, ##-- default: ['LtsSpecial']
##     Keep    => \@keepClassNames,    ##-- default: ['LtsKeep']
sub load_symbols {
  my ($lts,$file,%args) = @_;
  $lts = $lts->new() if (!ref($lts));

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak(__PACKAGE__, "::load(): open failed for symbols file '$file': $!") if (!$fh);

  my $cLetter  = $args{Letter}  ? $args{Letter}   : 'LtsLetter';
  my $cPhon    = $args{Phon}    ? $args{Phon}     : 'LtsPhon';
  my $cSpecial = $args{Special} ? $args{Special}  : 'LtsSpecial';
  my $cKeep    = $args{Keep}    ? $args{Keep}     : 'LtsKeep';

  my %hLetter  = map { $_=>undef } (ref($cLetter)  ? @$cLetter  : ($cLetter));
  my %hPhon    = map { $_=>undef } (ref($cPhon)    ? @$cPhon    : ($cPhon));
  my %hSpecial = map { $_=>undef } (ref($cSpecial) ? @$cSpecial : ($cSpecial));
  my %hKeep    = map { $_=>undef } (ref($cKeep)    ? @$cKeep    : ($cKeep));

  my ($class,@chars);
  while (<$fh>) {
    chomp;
    s/(?<!\\)\#.*//g;   ##-- ignore comments
    s/\\(.)/$1/g;       ##-- un-escape

    push(@{$lts->{symLines}}, $_); ##-- cache loaded symbols

    next if (/^\s*$/);  ##-- ... and blank lines

    $_ =~ s/^\s+//;     ##-- trim leading whitespace
    $_ =~ s/\s+$//;     ##-- ... and trailing whitespace

    ($class,@chars) = split(/\s+/, $_);
    @{$lts->{symLetters}}{@chars}  = undef if (exists($hLetter{$class}));
    @{$lts->{symPhones}} {@chars}  = undef if (exists($hPhon{$class}));
    @{$lts->{symSpecials}}{@chars} = undef if (exists($hSpecial{$class}));+
    @{$lts->{symKeep}}{@chars}     = undef if (exists($hKeep{$class}));
  }

  $fh->close if (!ref($file));
  return $lts;
}

##--------------------------------------------------------------
## Methods: I/O: Output: symbols

## $lts = $lts->save_symbols($symbols_filename_or_fh,%args)
##  + requires: lts_expaned_alphabet()
##  + %args:
##      Letter   => $LetterName,  ##-- symbol class name (default='LtsLetter')
##      Phon     => $PhonName,    ##-- symbol class name (default='LtsPhon')
##      Special  => $SpecialName  ##-- symbol class name (default='LtsSpecial')
##      Keep     => $KeepName,    ##-- symbol class name (default='LtsKeep')
##      Class    => $ClassPrefix, ##-- symbol class name prefix: false for no classes (default=none)
sub save_symbols {
  my ($lts,$file,%args) = @_;

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak(__PACKAGE__, "::save_symbols(): open failed for '$file': $!") if (!$fh);

  ##-- print cached symbols-file lines, if any
  $fh->print( (map { ($_,"\n") } @{$lts->{symLines}}), "\n") if ($lts->{symLines});

  ##-- get names
  my $Special = (exists($args{Special}) ? ($args{Special} ? $args{Special} : undef) : 'LtsSpecial');
  my $Letter  = (exists($args{Letter})  ? ($args{Letter}  ? $args{Letter}  : undef) : 'LtsLetter');
  my $Phon    = (exists($args{Phon})    ? ($args{Phon}    ? $args{Phon}    : undef) : 'LtsPhon');
  my $Keep    = (exists($args{Keep})    ? ($args{Keep}    ? $args{Keep}    : undef) : 'LtsKeep');

  ##-- print specials
  if (defined($Special)) {
    $lts->print_symbols_lines($fh, $Special, [
					      @SPECIALS,
					      (grep { !exists($SPECIALS{$_}) }
					       sort(keys(%{$lts->{specials}})))
					     ]);
      $fh->print("\n");
  }

  ##-- print letters [literals]
  if (defined($Letter)) {
    $lts->print_symbols_lines($fh, $Letter, [ sort(keys(%{$lts->{letters}})) ]);
    $fh->print("\n");
  }

  ##-- print keepers
  if (defined($Keep)) {
    $lts->print_symbols_lines($fh, $Keep,   [ sort(keys(%{$lts->{keep}})) ]);
    $fh->print("\n");
  }

  ##-- print phones
  if (defined($Phon)) {
    $lts->print_symbols_lines($fh, $Phon,   [ sort(keys(%{$lts->{phones}})) ]);
    $fh->print("\n");
  }

  ##-- print classes
  if ($args{ClassPrefix}) {
    my $classes = $lts->{classes};
    my ($c);
    foreach $c (sort(keys(%$classes))) {
      $lts->print_symbols_lines($fh, ($args{ClassPrefix}.$c), [ sort(keys(%{$classes->{$c}})) ]);
      $fh->print("\n");
    }
  }

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
## Methods: index generation: dual-ACPM Gfsm
##==============================================================================

##--------------------------------------------------------------
## Methods: index generation: Gfsm: Labels

## $labs = $lts->gfsmLabels
##   + requires: $lts->expand_alphabet()
sub gfsmLabels {
  my $lts = shift;
  my $labs = Gfsm::Alphabet->new;
  $labs->insert($_) foreach ('<epsilon>', map { sort keys %$_ } @$lts{qw(specials keep letters phones)});
  return $labs;
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
*gfsmFST = \&gfsmTransducer;
sub gfsmTransducer {
  my ($lts,%args) = @_;
  #require Gfsm;

  ##-------------------------
  ## LHS

  ##-- LHS: build left-context ACPM
  $lts->vmsg('progress',"LHS (trie");
  my $ltrie = subtrie($lts, which=>[qw(lhs)], rules=>$lts->{rules}, %args);

  $lts->vmsg0('progress', ', ACPM');
  my $lacpm  = Lingua::LTS::ACPM->newFromTrie($ltrie,joinout=>\&_acpm_joinout);

  $lts->vmsg0('progress', ', complete');
  $lacpm->complete();

  $lts->vmsg0('progress', ', expand');
  $lacpm->expand($lts->{classes}, packas=>'S', joinout=>\&Lingua::LTS::_acpm_joinout);

  $lts->vmsg0('progress', ', pack');
  $lacpm->packout(packas=>'S', packadd=>0);

  ##-- LHS: alphabets
  my $ilabs = $args{ilabels} ? $args{olabels} : Gfsm::Alphabet->new();
  $ilabs->insert('<epsilon>', 0) if (!defined($ilabs->find_key(0)));

  my $sharedlabs = Gfsm::Alphabet->new();
  $sharedlabs->insert('<epsilon>', 0);

  ##-- LHS: construct fst
  $lts->vmsg0('progress', ', FST');
  my $lfst = Gfsm::Automaton->new();
  $lfst->sort_mode(Gfsm::ASMNone());
  $lfst->is_transducer(1);
  $lfst->is_weighted(0);
  $lfst->root(0);
  my ($q,$qto,$gotoq,$c,$qoutp,$lo,$hi);
  foreach $q (0..($lacpm->{nq}-1)) {
    $gotoq = $lacpm->{goto}[$q];
    ##-- add arcs
    while (($c,$qto)=each(%$gotoq)) {
      $lo = $ilabs->get_label($c);
      $hi = $sharedlabs->get_label(pack('S', $lo).$lacpm->{out}{$q});
      $lfst->add_arc($q,$qto, $lo,$hi, 0);
    }
    ##-- check for state finality
    $lfst->is_final($q,1) if (exists($lacpm->{out}{$q}));
  }

  ##-- LHS: norule
  $lts->vmsg0('progress', ', norule');
  my $norulid = scalar(@{$lts->{rules}});
  $sharedlabs->insert(pack('SS', $_, $norulid)) foreach (1..($ilabs->size-1));

  ##-- LHS: keepers: arcs (q --a:a--> q) for q \in Q_{LHS}, c \in %keep
  my $nshared_packed = $sharedlabs->size;
  foreach $c (keys(%{$lts->{keep}})) {
    $lo = $ilabs->get_label($c);
    $hi = $sharedlabs->get_label(pack('SS', $lo, $norulid));
    foreach $q (0..($lfst->n_states-1)) {
      $lfst->add_arc($q,$q, $lo,$hi, 0);
    }
  }
  $lts->vmsg0('progress', "): done.\n");

  ##----------------------------
  ## IN+RHS

  ##-- LHS: build left-context ACPM
  $lts->vmsg('progress',"RHS (trie");
  my $rtrie = subtrie($lts, which=>[qw(in rhs)], reversed=>1, rules=>$lts->{rules}, %args);

  $lts->vmsg0('progress', ', ACPM');
  my $racpm  = Lingua::LTS::ACPM->newFromTrie($rtrie,joinout=>\&_acpm_joinout);

  $lts->vmsg0('progress', ', complete');
  $racpm->complete();

  $lts->vmsg0('progress', ', expand');
  $racpm->expand($lts->{classes}, packas=>'S', joinout=>\&Lingua::LTS::_acpm_joinout);

  $lts->vmsg0('progress', ', pack');
  $racpm->packout(packas=>'S', packadd=>0);

  ##-- IN+RHS: alphabets
  my $rullabs = Gfsm::Alphabet->new();
  $rullabs->insert('<epsilon>', 0);
  $rullabs->insert($lts->{rules}[$_], $_+1) foreach (0..$#{$lts->{rules}});
  $rullabs->insert({lhs=>[],rhs=>[],in=>[],out=>[],id=>$norulid}, $norulid+1);    ##-- <norule>
  my $nrullabs_ids = $rullabs->size;
  ##-- <keep=STR>
  $rullabs->insert("<keep=$_>") foreach (sort(keys(%{$lts->{keep}})));

  ##-- IN+RHS: indexing
  $lts->vmsg0('progress', ', index');
  my $cr2shared = {}; ##-- maps ($ilab.' '.($rulid+0)) => \@sharedlabs_for_char_containing_rulid
  my $ilabs_a      = $ilabs->asArray;
  my $ilabs_h      = $ilabs->asHash;
  my $sharedlabs_a = $sharedlabs->asArray;
  my $sharedlabs_h = $sharedlabs->asHash;
  my ($ilab,$rulid,@rulids,$sharedlab);
  foreach $sharedlab (1..($nshared_packed-1)) {
    ($ilab,@rulids) = unpack('S*', $sharedlabs_a->[$sharedlab]);
    foreach $rulid (@rulids) {
      push(@{$cr2shared->{$ilab.' '.$rulid}}, $sharedlab);
    }
  }

  ##-- IN+RHS: fst construction
  $lts->vmsg0('progress', ', FST');
  my $rfst = Gfsm::Automaton->new();
  $rfst->sort_mode(Gfsm::ASMNone());
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
	if ($qto != 0 && !exists($warnedabout{$qto})) {
	  $lts->vmsg0('error', "\n");
	  $lts->vmsg('error', "no output defined for RACPM state q$qto -- using <norule>!\n");
	  $warnedabout{$qto} = undef;
	}
	#$rfst->add_arc($q,$qto, $sharedlabs->get_label(''),$rullabs->get_label('<norule>'), 0);
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

  ##-- RHS: reverse
  $lts->vmsg0('progress', ', reverse');
  my $rrfst = $rfst->reverse;
  $lts->vmsg0('progress', "): done.\n");

  ##-------------------------
  ## Ouput filter

  ##-- output filter: labels
  $lts->vmsg('progress', "Output Filter: (labels");
  my $folabs = $args{olabels} ? $args{olabels} : Gfsm::Alphabet->new();
  $folabs->insert('<epsilon>',0) if (!defined($folabs->find_key(0)));
  $folabs->insert($_) foreach (sort(keys(%{$lts->{phones}})));

  ##-- output filter: fst
  $lts->vmsg0('progress', ', FST');
  my $filter = Gfsm::Automaton->new;
  $filter->is_transducer(1);
  $filter->is_weighted(0);
  $filter->root(0);
  $filter->is_final(0,1);
  $filter->add_arc(0,0, $norulid+1,0, 0); ##-- <norule> (?)
  my @consume = (0,0);
  my ($rul,$inlen,$rulout,$qfrom,$clen,$rullab,$i);
  my $qmax = 1;
  foreach $rul (@{$lts->{rules}}) {
    $rulid = $rul->{id};
    $inlen = @{$rul->{in}};

    ##-- get consume path: ( $consume[$inlen] -- (AnyRule:<eps>)^${inlen} --> 0 )
    if (!defined($consume[$inlen])) {
      ##-- add consume states from $#consume..$inlen
      foreach $clen (grep { !defined($consume[$_]) } (1..$inlen)) {
	$qfrom = $consume[$clen] = $qmax++;
	$qto   = $consume[$clen-1];

	##-- add an arc from consume($n) to consume($n-1) for each output rule (including null-rule)
	foreach $rullab (1..($#{$lts->{rules}}+2)) {
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
  $lts->vmsg0('progress', "): done.\n");

  ##-------------------------
  ## Algebra: LHS ° rev(RHS) ° Filter

  $lts->vmsg('progress', "Composition: (sort");
  $lfst->arcsort(Gfsm::ASMUpper());
  $rrfst->arcsort(Gfsm::ASMLower());
  $filter->arcsort(Gfsm::ASMUpper());

  $lts->vmsg0('progress', ', LHS @ RHS');
  my $cfst = $lfst->compose($rrfst);

  $lts->vmsg0('progress', ' @ filter');
  $cfst->arcsort(Gfsm::ASMUpper());
  my $fcfst = $cfst->compose($filter);
  $fcfst->arcsort(Gfsm::ASMLower());
  $lts->vmsg0('progress', "): done.\n");

  ##-------------------------
  ## RETURN
  @$lts{qw(fst fstilabs fstolabs)} = ($fcfst,$ilabs,$folabs);
  return wantarray ? ($fcfst,$ilabs,$folabs) : $fcfst;
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

  my $rules = defined($args{rules}) ? $args{rules} : $lts->{rulex};
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
## Methods: Rule sanitizing
##==============================================================================

## $lts = $lts->sanitize_rules
##  + instantiates default rules ( [ x ] =   ) for each unhandled letter x
##  + instantiates default rules ( [ X ] = X ) for each unhandled special X
##  + requirs: $lts->expand_alphabet()
sub sanitize_rules {
  my $lts = shift;

  ##-- get pseudo-set of handled characters
  my $rules = $lts->{rules};
  my %letters  = ( %{$lts->{letters}}, );
  my %specials = ( %{$lts->{specials}}, );
  my %syms     = ( %letters, %specials );
  delete(@syms{keys(%{$lts->{keep}})});
  my %handled = qw();

  my ($rul);
  foreach $rul (grep { @{$_->{lhs}}==0 && @{$_->{rhs}}==0 && @{$_->{in}}==1 } @$rules) {
    $handled{$rul->{in}[0]} = undef;
  }

  ##-- add rules for unhandled letters
  my ($c);
  delete(@letters{keys(%handled)});
  my $id = scalar(@$rules);
  foreach $c (sort keys(%letters)) {
    push(@$rules, {lhs=>[],in=>[$c],rhs=>[],out=>[],id=>$id});
    ++$id;
  }

  ##-- add rules for unhandled specials
  delete(@specials{keys(%handled)});
  foreach $c (sort keys(%specials)) {
    push(@$rules, {lhs=>[],in=>[$c],rhs=>[],out=>[$c],id=>$id});
    ++$id;
  }

  return $lts;
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
##   + instantiates {letters}, {phones}, {specials} keys from rules and {sym*} keys
sub expand_alphabet {
  my $lts = shift;
  my ($letters,$phones,$specials,$keep) = @$lts{qw(letters phones specials keep)} = ({},{},{},{});

  ##-- get letters, phones, specials from symbols file (if any)
  @$letters  {keys(%{$lts->{symLetters }})} = undef if ($lts->{symLetters });
  @$phones   {keys(%{$lts->{symPhones  }})} = undef if ($lts->{symPhones  });
  @$specials {keys(%{$lts->{symSpecials}})} = undef if ($lts->{symSpecials});
  @$keep     {keys(%{$lts->{symKeep}})}     = undef if ($lts->{symKeep    });

  ##-- get letters from classes
  @$letters{map { keys(%$_) } values(%{$lts->{classes}})} = undef;

  ##-- get pre-defined keepers, specials & phones from loaded .lts file
  @$phones{keys %{$lts->{phones0}}} = undef;
  @$specials{keys %{$lts->{special0}}} = undef;
  @$keep{keys %{$lts->{keep0}}} = undef;

  ##-- get letters & phones from rules
  my ($r,$part);
  foreach $r (@{$lts->{rules}}) {
    foreach $part (@$r{qw(lhs rhs in)}) {
      @$letters{@$part} = undef;
    }
    @$phones{@{$r->{out}}} = undef;
  }

  ##-- specials, keeper, & classes are neither letters nor phones
  @$specials{@SPECIALS}=undef; ##-- always special
  delete(@$letters{keys(%$specials), keys(%$keep), keys(%{$lts->{classes}}) });
  delete(@$phones{keys(%$specials), keys(%$keep)});

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
  my ($lts,$wchars,$pos0) = @_;
  $pos0 = 0 if (!defined($pos0));

  my $rules = $lts->{rules};
  my $classes = $lts->{classes};

  my ($rule,@phones,$newpos);
  ##-- scan for keep-characters
  my @keep = qw();
  my $offset=0;
  my ($pos);
  for ($pos=$pos0; $pos <= $#$wchars; ) {
    if (exists($lts->{keep}{$wchars->[$pos]})) {
      ##-- keeper
      if ($lts->{apply_verbose}) {
	my $vword = join('', @$wchars[0..$pos-1], '_', @$wchars[$pos..$#$wchars]);
	print STDERR "Keep: \'$vword' matches keep character: '$wchars->[$pos]'\n";
      }
      ++$offset;
      push(@{$keep[$pos-$offset]}, splice(@$wchars, $pos, 1));
    }
    ++$pos;
  }

  $pos=$pos0;
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

	while ($pos < $newpos) {
	  push(@phones, @{$keep[$pos]}) if ($keep[$pos]);
	  ++$pos;
	}
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


__END__

##==============================================================================
## PODS
##==============================================================================
=pod

=head1 NAME

Lingua::LTS - compiler/interpreter for festival-style letter-to-sound rules

=head1 SYNOPSIS

 ##-------------------------------------------------------------
 ## Requirements
 use Gfsm;         ##-- requires version >= 0.207
 use Lingua::LTS;

 ##-------------------------------------------------------------
 ## Constructors, Destructors, etc.
 $class = 'Lingua::LTS';
 $lts   = $class->new(%args);    ##-- new object

 ##-------------------------------------------------------------
 ## Methods: I/O

 $lts = $lts->load($file);          ##-- load an .lts file
 $lts = $class->load($file);        ##-- ... into a new object

 $lts = $lts->load_symbols($file,%args);   ##-- load symbols file
 $lts = $class->load_symbols($file,%args); ##-- ... into new obj

 $lts = $lts->save_symbols($file,%args);   ##-- save symbols file

 ##-------------------------------------------------------------
 ## Methods: compilation: general (expansion)

 $lts = $lts->expand_alphabet();  ##-- expand character classes (OFTEN REQUIRED)
 $lts = $lts->expand_rules();     ##-- expand class-based rules (OFTEN REQUIRED)

 $lts = $lts->sanitize_rules();   ##-- various sanity checks (RECCOMMENDED)

 ##-------------------------------------------------------------
 ## Methods: compilation: Aho-Corasick Pattern Matcher

 $acpm = $lts->toACPM(%args);        ##-- Step 1: Lingua::LTS::ACPM object

 ##-------------------------------------------------------------
 ## Methods: compilation: Gfsm Finite-State Transducer

 $lab = $lts->gfsmLabels();          ##-- Step 2: Gfsm::Alphabet object
 $fst = $lts->gfsmTransducer(%args); ##-- Step 3: Gfsm::Automaton object

 ##-------------------------------------------------------------
 ## Methods: Lookup

 @phones = $lts->apply_word_indexed_acpm($word); ##-- via ACPM
 @phones = $lts->apply_word($word);              ##-- linear search (SLOW!)
 @phones = $lts->apply_chars(\@chars);           ##-- linear search (SLOW!)

 ##-------------------------------------------------------------
 ## Methods: Information

 $info = $lts->info();  ##-- informational HASH-ref


=cut

##==============================================================================
## Description
##==============================================================================
=pod

=head1 DESCRIPTION

Lingua::LTS provides an object-oriented compiler/interpreter for
deterministic letter-to-sound rules with festival-like syntax and semantics
(see papers and links below for details).

=cut

##==============================================================================
## Methods
##==============================================================================
=pod

=head1 METHODS

=cut

##-------------------------------------------------------------
## Constructors, Destructors, etc.
=pod

=head2 Constructors, Destructors, etc.

=over 4

=item $lts = Lingua::LTS->new(%args)

Creates and returns a new Lingua::LTS object.  Object returned is a blessed HASH-ref.
C<%args> may contain the following options (and more):

  ##-- Options
  implicit_bos => $bool,     ##-- default=1
  implicit_eos => $bool,     ##-- default=1

  ##-- debugging
  apply_verbose => $bool,    ##-- be verbose about linear search rule application?
  apply_warn=>$bool,         ##-- emit warning messages when applying rules?
  verbose=>$level,           ##-- for operations which might take a long time

=back

=cut


##-------------------------------------------------------------
## Methods: I/O
=pod

=head2 Methods: I/O

=over 4

=item $lts = $lts->load($file)

=item $lts = Lingua::LTS->load($file)

Loads an .lts file into a (new) Lingua::LTS object. 
C<$file> may be either a filename or an open filehandle.
See L<LTS File Syntax> for details on .lts file syntax.


=item $lts = $lts->load_symbols($file,%args)

=item $lts = $class->load_symbols($file,%args)

Loads a .sym file (AT&T lextools format) into an existing Lingua::LTS object.
C<$file> may be either a filename or an open filehandle.
The following options may be passed in %args:

  Letter  => \@letterClassNames,  ##-- default: ['LtsLetter']
  Phon    => \@phonClassNames,    ##-- default: ['LtsPhon']
  Special => \@specialClassNames, ##-- default: ['LtsSpecial']
  Keep    => \@keepClassNames,    ##-- default: ['LtsKeep']

... this allows manipulation of the LTS alphabet via a non-LTS syntax.


=item $lts = $lts->save_symbols($file,%args)

Requires: expand_alphabet()

Saves a .sym file (AT&T lextools format) containing the alphabet of
a Lingua::LTS object.
C<$file> may be either a filename or an open filehandle.
The following options may be passed in %args:

  Letter  => \@letterClassNames,  ##-- default: ['LtsLetter']
  Phon    => \@phonClassNames,    ##-- default: ['LtsPhon']
  Special => \@specialClassNames, ##-- default: ['LtsSpecial']
  Keep    => \@keepClassNames,    ##-- default: ['LtsKeep']
  Class   => $ClassPrefix, ##-- symbol class name prefix: false for no classes (default=none)

=back

=cut


##-------------------------------------------------------------
## Methods: compilation: general (expansion)
=pod

=head2 Methods: compilation: general (expansion)

=over 4


=item $lts = $lts->expand_alphabet()

Instantiates internal alphabet structures stored in keys
{letters}, {phones}, and {specials}.
Alphabet information is expanded based on rules and {sym*}
keys (as read from .sym file, if any).

Required by many methods.


=item $lts = $lts->expand_rules()

Requires: expand_alphabet()

Expands all class names occurring in {rules} into all
literal characters of the corresponding class.  Expanded
rules are stored in {rulex}.

Required by many methods.


=item $lts = $lts->sanitize_rules()

Requires: expand_alphabet()

Performs some sanity checks on defined rules.  Highly reccommended.

=over 4

=item *

instantiates default rules ( [ x ] =   ) for each unhandled letter x

=item *

instantiates default rules ( [ X ] = X ) for each unhandled special X

=back


=back

=cut


##-------------------------------------------------------------
## Methods: compilation: Aho-Corasick Pattern Matcher
=pod

=head2 Methods: compilation: Aho-Corasick Pattern Matcher

=over 4

=item $acpm = $lts->toACPM(%args)

Requires: expand_alphabet(), expand_rules()

Compiles an Aho-Corasick Pattern Matcher as a Lingua::LTS::ACPM object from the rules in $lts.
Compiled ACPM is cached in C<$lts-E<gt>{acpm}>.

=back

=cut


##-------------------------------------------------------------
## Methods: compilation: Gfsm Finite-State Transducer
=pod

=head2 Methods: compilation: Gfsm Finite-State Transducer

=over 4

=item $lab = $lts->gfsmLabels()

Requires: expand_alphabet()

Returns a Gfsm::Alphabet object representing the terminal labels
used by C<$lts>.


=item $fst = $lts->gfsmTransducer(%args)

Requires: toACPM()

Compiles and returns a Gfsm::Automaton transducer for deterministic
application of the LTS ruleset.
Returned transducer is cached in C<$lts-E<gt>{fst}>,
its input labels are cached in C<$lts-E<gt>{fstilabs}>,
and its output labels are cached in C<$lts-E<gt>{fstolabs}>.
Valid options in %args:

  ilabels=>$ilabs, ##-- default = $lts->gfsmLabels()
  olabels=>$olabs, ##-- default = $ilabs

=back

=cut


##-------------------------------------------------------------
## Methods: Lookup
=pod

=head2 Methods: Lookup

=over 4

=item @phones = $lts->apply_word_indexed_acpm($word)

Requires: toACPM()

Apply LTS ruleset to word $word (a string), using compiled ACPM
for indexed lookup.  Really only useful for testing.


=item @phones = $lts->apply_word($word)

Requires: expand_alphabet() ?

Apply LTS ruleset to word $word (a string), using linear search
over all (expanded) rules.  Very slow.  Only really useful for testing.

=item @phones = $lts->apply_chars(\@chars)

Requires: expand_alphabet() ?

Apply LTS ruleset to an array of characters \@chars, using linear search
over all (expanded) rules.  Very slow.  Only really useful for testing.


=back

=cut


##-------------------------------------------------------------
## Methods: Information
=pod

=head2 Methods: Information

=over 4

=item $info = $lts->info()

Returns a HASH-reference containing some summary information about the LTS object.

=back

=cut

##==============================================================================
## Footer
##==============================================================================
=pod

=head1 LTS File Syntax

LTS rulefiles as read by the Lingua::LTS::load() method do not read
festival SCHEME syntax directly, but rather a related format.  It is almost
trivial to convert festival SCHEME syntax to Lingua::LTS syntax, but
some decisions -- particularly involving class membership and class-based rules --
must be made by a human being.  The syntax for LTS files is given in pseudo-BNF
notation below:

  LTS_FILE ::= LTS_LINE*
  LTS_LINE ::= ( BLANK | COMMENT | PHON | SPECIAL | KEEP | CLASS | IGNORE | RULE ) "\n"
  BLANK    ::= (whitespace)
  COMMENT  ::= ";" (anything)
  PHON     ::= "phon" PHONSYMS
  SPECIAL  ::= "special" SPECIALSYMS
  KEEP     ::= "keep" KEEPSYMS
  CLASS    ::= "class" CLASS_NAME CHARS
  CLASS_NAME ::= (string)
  CHARS      ::= ((string) | "#") *
  IGNORE     ::= "ignore" CHARS
  RULE       ::= RULE_LHS "[" RULE_IN "]" RULE_RHS "=" RULE_OUT

=cut


##==============================================================================
## Footer
##==============================================================================
=pod

=head1 SEE ALSO

=over 4

=item *

A. Black and P. Taylor, "Festival Speech Synthesis System".
Technical Report HCRC/TR-83,
University of Edinburgh, Centre for Speech Technology Research,
1997.

URL: http://www.cstr.ed.ac.uk/projects/festival

=item *

G. Möhler, A. Schweitzer, and Mark Breitenbücher,
"{IMS} {G}erman {F}estival Manual, version 1.2".
Institut für Maschinelle Sprachverarbeitung, Universität Stuttgart,
17 July, 2001.

URL: http://www.ims.uni-stuttgart.de/phonetik/synthesis


=item lts2fst.perl

=item lts2sym.perl

=item lts-apply.perl

=item lts-info.perl

=item Lingua::LTS::Trie(3perl)

=item Lingua::LTS::ACPM(3perl)

=item Lingua::LTS::Automaton(3perl)

=item Lingua::LTS::Gfsm(3perl)

=item Gfsm(3perl)

URL: http://www.ling.uni-potsdam.de/~moocow/projects/gfsm

=back



=head1 AUTHOR

Bryan Jurish E<lt>moocow@bbaw.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005-2006 by Bryan Jurish

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
