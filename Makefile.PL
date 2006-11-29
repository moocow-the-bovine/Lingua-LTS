use ExtUtils::MakeMaker;

##----------------------------------------------------------------------
## MAIN
##----------------------------------------------------------------------
WriteMakefile
  (
   NAME		=> 'Lingua::LTS',
   AUTHOR       => 'Bryan Jurish <moocow@bbaw.de>',
   ABSTRACT	=> 'Festival-style letter-to-sound ruleset utilities',
   VERSION_FROM	=> 'LTS.pm',

   PREREQ_PM    => {
		    'Gfsm'=>0.0207,
		    'Encode'=>undef,
		    'Tie::Cache'=>undef,
		    'File::Basename'=>undef,
		    'Getopt::Long'=>undef,
		    'Pod::Usage'=>undef,
		    'Storable'=>undef,
		    'Time::HiRes'=>undef,
		   },

   EXE_FILES    => [qw(lts2fst.perl lts2sym.perl),
		    qw(lts-apply.perl lts-info.perl),
		   ],
  );


##-----------------------------------------------------------------------
## Extensions
##-----------------------------------------------------------------------

sub MY::postamble {
  # how to format the README file
  "README.txt : README.POD\n"
  ."\tpod2text README.POD README.txt\n\n";
}

sub MY::dist_core {
  package MY;
  my $inherited = shift->SUPER::dist_core(@_);
  $inherited =~
    # ... distribution requires current README file
    s/^dist\s*:/dist\ : README.txt/m;
  $inherited;
}
