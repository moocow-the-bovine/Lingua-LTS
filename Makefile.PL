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

   EXE_FILES    => [qw(lts-apply.perl lts2rul.perl)],
  );


#-----------------------------------------------------------------------
# Extensions
#-----------------------------------------------------------------------
