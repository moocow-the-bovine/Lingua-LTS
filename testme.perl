#!/usr/bin/perl -wd

use lib qw(.);
use Lingua::LTS;

##--------------------------------------------------------------
## Test cases: test1
sub test1 {
  our $lts = Lingua::LTS->new;
  $lts->load('test.lts');
}

##--------------------------------------------------------------
## brute force index
sub testbfi {
  my %args = @_;
  $args{view} = 1 if (!defined($args{view}));

  $lts->expand_alphabet;
  $lts->expand_rules;
  $lts->bruteForceGfsmIndex;

  my $lsta_qlab = Gfsm::Alphabet->new;
  $lsta_qlab->insert("$_:".join(',', unpack('L*', $lts->{lsta2rules}[$_])), $_)
    foreach (0..$#{$lts->{lsta2rules}});
  $lts->{lsta}->viewps(labels=>$lts->{bfLabs},states=>$lsta_qlab,title=>'leftSTA',bg=>1)
    if ($args{view});

  my $rpta_qlab = Gfsm::Alphabet->new;
  $rpta_qlab->insert("$_:".join(',', unpack('L*', $lts->{rpta2rules}[$_])), $_)
    foreach (0..$#{$lts->{rpta2rules}});
  $lts->{rpta}->viewps(labels=>$lts->{bfLabs},states=>$rpta_qlab,title=>'rightPTA',bg=>1)
    if ($args{view});
}
#test1;
#testbfi;

##--------------------------------------------------------------
## brute force search
sub testbrute {
  testbfi(view=>0);
  $lts->bruteForceSearch();
}
test1;
testbrute;


##-- main: dummy
foreach $i (0..5) { print("--dummy[$i]--\n"); }
