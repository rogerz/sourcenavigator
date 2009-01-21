$! This procedure makes a complete FLOPPY program for
$! compilation on IBM/CMS.
$ copy *.for/excl=(vmsfloppy.for,unixfloppy.for) floppy.fortran/log
