$! This procedure creates FLOPPY.EXE from its component Fortran
$! source files.
$! It assumes it is being executed in a dedicated [.floppy] subdirectory.
$ foropt = ""
$ if f$getsyi("Arch_Name") .eqs. "Alpha" then foropt = "/NOWARN=UNCALLED"
$loop1:
$ file = f$search("*.for")
$ if file .eqs. "" then goto end_loop1
$ if f$locate("UNIX",file) .ne. f$length(file) then goto loop1
$ if f$locate("CMSFLOPPY",file) .ne. f$length(file) then goto loop1
$ write sys$output "Compiling ''file'"
$ for'foropt' 'p1' 'file
$ goto loop1
$end_loop1:
$ if f$search("floppy.olb") .nes. "" then delete floppy.olb;*
$ library/create floppy.olb *.obj
$ library/delete=floppy floppy
$ write sys$output "Linking ...."
$ link/exe=floppy 'p2' vmsfloppy,floppy/lib
$ delete *.obj.*
$ directory/size=alloc floppy.exe
$ write sys$output "Finished."
$ exit
