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
## Test: symbols
sub symtest {
  test1;
  $lts->load_symbols('symtest.sym');
  $lts->expand_alphabet();
  $lts->save_symbols('-');
}
symtest();



##-- main: dummy
foreach $i (0..5) { print("--dummy[$i]--\n"); }
