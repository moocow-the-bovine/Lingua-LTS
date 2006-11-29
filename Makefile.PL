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

   EXE_FILES    => [qw(lts-apply.perl lts2rul.perl)],
  );


#-----------------------------------------------------------------------
# Extensions
#-----------------------------------------------------------------------
