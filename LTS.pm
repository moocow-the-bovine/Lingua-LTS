## File: Lingua::LTS.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: festival-style letter-to-sound rules


package Lingua::LTS;
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
  my ($lts,$fh,$class,$syms) = @_;
  my ($line);
  while (@$syms) {
    $line = "$class\t";
    while (@$syms && length($line) + length($syms->[0]) < 80) {
      $line .= ' '.shift(@$syms);
    }
    $fh->print($line,"\n");
  }
}



##--------------------------------------------------------------
## Methods: I/O: Output: AT&T context-sensitive rewrite rules

## undef = $lts->to_csrules($filename_or_fh)

##------ PROBLEM (solved):
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
  $fh->print("simultaneous\n\n");

  my $rules = $lts->{rules};
  my ($r);
  my $i=1;
  foreach $r (@$rules) {
    $lts->print_csrule_line_x($fh, @$r{qw(lhs in out rhs)}, "RULE ".($i++).": ", rule2str($r));
  }

  ##-- print FINAL special rule (bos/eos)
#  $fh->print("\n");
#  $lts->print_csrule_line_x($fh,
#		      [],['#'],['<epsilon>'],[],
#		      "DUMMY RULE: eliminate BOS/EOS markers");
#  $lts->print_csrule_line_x($fh,
#		      [], ['=<epsilon>'],['<epsilon>'],[],
#		      "DUMMY RULE: eliminate phonetic silence");

  $fh->close if (!ref($file));
  return $lts;
}

## undef = print_csrule_line_x($fh,\@left,\@in,\@out,\@right,@comments)
sub print_csrule_line_x {
  my ($lts,$fh,$lc,$in,$out,$rc,@comments) = @_;
  my ($lenPhi,$lenPsi,$lenL,$lenR) = (8,16,12,12);
  $fh->print(
	     sprintf("%${lenPhi}s -> %-${lenPsi}s / %${lenL}s __ %-${lenR}s  %s\n",
		     ##-- input (target: Phi)
		     join(' ', map { $lts->att_str($_) } @$in),

		     ##-- output (target: Psi)
		     join(' ',
			  ##-- output: history
			  (map { $lts->att_str($_,'-') } @$in),
			  ##-- output: phones
			  (map { $lts->att_str($_,'=') } @$out),
			 ),

		     ##-- left context (history)
		     join(' ',
			  map { ($lts->att_str((exists($lts->{classes}{$_}) ? "Class$_" : $_),
					       '-'), ##-- history
				 '([Phon]*)')
			       } @$lc),

		     ##-- right context
		     join(' ',
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

## @phones = $lts->apply_chars($lts,\@word_chars)
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
	  print STDERR "Match: \'$vword' matches rule ", rule2str($rule), "\n";
	}

	push(@phones,@{$rule->{out}});
	$pos = $newpos;
	next CHAR;
      }
    }
    ##-- no match
    my $errword = join('', @$wchars[0..$pos-1], '<<HERE>>', @$wchars[$pos..$#$wchars]);
    warn("$0: could not translate word \`$errword' -- skipping");
    last;
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
  $info->{'alpha_nClasses'} = scalar(keys(%{$lts->{classes}}));

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
