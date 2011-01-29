use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile being created.

sub MY::co {
'
.cc.o:
	CC -PIC -DDEBUGGING -Bdynamic -g -c $(DEFINE) -I$(TOP) $(INC) $*.cc
';
}
#sub MY::co { $_=MM->co; s/\.c/.cc/g; $_ };
sub MY::c { $_=MM->c; s/\.c/\.cc/; $_ };
sub MY::clean { $_=MM->clean; s/\.c/\.cc/; $_ };
#sub MY::dynamic { $_=MM->dynamic; s/	ld/	CC -Bdynamic -Qoption ld -znodefs/; $_};

###
### If you get an error that looks like:
###
###
###	- Can't load '/home/muse/dschieb/src/perl/lib/sun4/solaris/auto/Glish/Glish.so' for module Glish: ld.so.1: /home/muse/dschieb/src/perl/bin/sun4/solaris/perl: fatal: relocation error: symbol not found: __pure_virtual_called: referenced in /net/kochab/kochab_5/dschieb/glish-2.5.0.2/lib/sun4sol.so/libglish.so at /home/muse/dschieb/src/perl/lib/DynaLoader.pm line 124.
###
### 	at /home/muse/dschieb/src/perl/lib/Glish.pm line 9
###	BEGIN failed--compilation aborted at ./try line 8.
###	error, new connection immediately broken
###
### when you attempt to load a Glish client, it probably means that you are
### lacking some dynamic library. This particular error, for me, means that
### I have to hand edit the generated Makefile and add in:
###
###		/opt/SUNWspro/SC2.0.1/libC.so.3
###
### which for some reason gets stripped out.
&writeMakefile(
#	'potential_libs' => '-L/home/muse/dschieb/dev/glish-2.5.0/glish/sparc-sunsol-cfront.so -L/home/muse/dschieb/dev/glish-2.5.0/sds/sparc-sunsol-cfront.so -L/opt/local/gnu/lib/gcc-lib/sparc-sun-solaris2.3/2.6.0 -L/opt/local/gnu/lib -lglish -lsds -lg++ -lgcc',
#	'potential_libs' => '-L/home/muse/dschieb/dev/glish-2.5.0/glish/sparc-sunsol-cfront.so -L/home/muse/dschieb/dev/glish-2.5.0/sds/sparc-sunsol-cfront.so /opt/SUNWspro/SC2.0.1/libC.so.3 -lglish -lsds',
##
##	'potential_libs' => '-L/home/muse/dschieb/dev/glish-2.5.0/glish/sparc-sunsol-cfront.so /net/muse/muse/salty/sparc-sunsol-cfront.so/libsds.a /SUNWspro/SC2.0.1/libC.so.3 -lglish',
##	'INC' => '-I/home/muse/dschieb/dev/glish-2.5.0/include',
##
	'potential_libs' => '-L/net/kochab/kochab_5/dschieb/glish-2.5.0.2/lib/sun4sol.so -L/opt/SUNWspro/SC2.0.1 -lglish -lsds -lC',
	'INC' => '-I/net/kochab/kochab_5/dschieb/glish-2.5.0.2/include',
	'DISTNAME' => 'Glish',
	'VERSION' => '0.1',
);
