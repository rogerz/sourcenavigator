	.file	"gzip.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.section ".data"
	.align 2
	.type	 license_msg,@object
license_msg:
	.long .LC0
	.long .LC1
	.long .LC2
	.long .LC3
	.long .LC4
	.long .LC5
	.long .LC6
	.long .LC7
	.long .LC8
	.long .LC9
	.long .LC5
	.long .LC10
	.long .LC11
	.long .LC12
	.long 0
	.section	".rodata"
	.align 2
.LC12:
	.string	"   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA."
	.align 2
.LC11:
	.string	"   along with this program; if not, write to the Free Software"
	.align 2
.LC10:
	.string	"   You should have received a copy of the GNU General Public License"
	.align 2
.LC9:
	.string	"   GNU General Public License for more details."
	.align 2
.LC8:
	.string	"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
	.align 2
.LC7:
	.string	"   but WITHOUT ANY WARRANTY; without even the implied warranty of"
	.align 2
.LC6:
	.string	"   This program is distributed in the hope that it will be useful,"
	.align 2
.LC5:
	.string	""
	.align 2
.LC4:
	.string	"   any later version."
	.align 2
.LC3:
	.string	"   the Free Software Foundation; either version 2, or (at your option)"
	.align 2
.LC2:
	.string	"   it under the terms of the GNU General Public License as published by"
	.align 2
.LC1:
	.string	"   This program is free software; you can redistribute it and/or modify"
	.align 2
.LC0:
	.string	"   Copyright (C) 1992-1993 Jean-loup Gailly"
	.size	 license_msg,60
	.globl ascii
	.section ".sdata","aw"
	.align 2
	.type	 ascii,@object
	.size	 ascii,4
ascii:
	.long 0
	.globl to_stdout
	.align 2
	.type	 to_stdout,@object
	.size	 to_stdout,4
to_stdout:
	.long 0
	.globl decompress
	.align 2
	.type	 decompress,@object
	.size	 decompress,4
decompress:
	.long 0
	.globl force
	.align 2
	.type	 force,@object
	.size	 force,4
force:
	.long 0
	.globl no_name
	.align 2
	.type	 no_name,@object
	.size	 no_name,4
no_name:
	.long -1
	.globl no_time
	.align 2
	.type	 no_time,@object
	.size	 no_time,4
no_time:
	.long -1
	.globl recursive
	.align 2
	.type	 recursive,@object
	.size	 recursive,4
recursive:
	.long 0
	.globl list
	.align 2
	.type	 list,@object
	.size	 list,4
list:
	.long 0
	.globl verbose
	.align 2
	.type	 verbose,@object
	.size	 verbose,4
verbose:
	.long 0
	.globl quiet
	.align 2
	.type	 quiet,@object
	.size	 quiet,4
quiet:
	.long 0
	.globl do_lzw
	.align 2
	.type	 do_lzw,@object
	.size	 do_lzw,4
do_lzw:
	.long 0
	.globl test
	.align 2
	.type	 test,@object
	.size	 test,4
test:
	.long 0
	.globl maxbits
	.align 2
	.type	 maxbits,@object
	.size	 maxbits,4
maxbits:
	.long 16
	.globl method
	.align 2
	.type	 method,@object
	.size	 method,4
method:
	.long 8
	.globl level
	.align 2
	.type	 level,@object
	.size	 level,4
level:
	.long 6
	.globl exit_code
	.align 2
	.type	 exit_code,@object
	.size	 exit_code,4
exit_code:
	.long 0
	.globl args
	.align 2
	.type	 args,@object
	.size	 args,4
args:
	.long 0
	.globl total_in
	.align 2
	.type	 total_in,@object
	.size	 total_in,4
total_in:
	.long 0
	.globl total_out
	.align 2
	.type	 total_out,@object
	.size	 total_out,4
total_out:
	.long 0
	.globl remove_ofname
	.align 2
	.type	 remove_ofname,@object
	.size	 remove_ofname,4
remove_ofname:
	.long 0
	.globl longopts
	.section ".data"
	.align 2
	.type	 longopts,@object
longopts:
	.long .LC13
	.long 0
	.long 0
	.long 97
	.long .LC14
	.long 0
	.long 0
	.long 99
	.long .LC15
	.long 0
	.long 0
	.long 99
	.long .LC16
	.long 0
	.long 0
	.long 100
	.long .LC17
	.long 0
	.long 0
	.long 100
	.long .LC18
	.long 0
	.long 0
	.long 102
	.long .LC19
	.long 0
	.long 0
	.long 104
	.long .LC20
	.long 0
	.long 0
	.long 108
	.long .LC21
	.long 0
	.long 0
	.long 76
	.long .LC22
	.long 0
	.long 0
	.long 110
	.long .LC23
	.long 0
	.long 0
	.long 78
	.long .LC24
	.long 0
	.long 0
	.long 113
	.long .LC25
	.long 0
	.long 0
	.long 113
	.long .LC26
	.long 0
	.long 0
	.long 114
	.long .LC27
	.long 1
	.long 0
	.long 83
	.long .LC28
	.long 0
	.long 0
	.long 116
	.long .LC29
	.long 0
	.long 0
	.long 84
	.long .LC30
	.long 0
	.long 0
	.long 118
	.long .LC31
	.long 0
	.long 0
	.long 86
	.long .LC32
	.long 0
	.long 0
	.long 49
	.long .LC33
	.long 0
	.long 0
	.long 57
	.long .LC34
	.long 0
	.long 0
	.long 90
	.long .LC35
	.long 1
	.long 0
	.long 98
	.long 0
	.long 0
	.long 0
	.long 0
	.section	".rodata"
	.align 2
.LC35:
	.string	"bits"
	.align 2
.LC34:
	.string	"lzw"
	.align 2
.LC33:
	.string	"best"
	.align 2
.LC32:
	.string	"fast"
	.align 2
.LC31:
	.string	"version"
	.align 2
.LC30:
	.string	"verbose"
	.align 2
.LC29:
	.string	"no-time"
	.align 2
.LC28:
	.string	"test"
	.align 2
.LC27:
	.string	"suffix"
	.align 2
.LC26:
	.string	"recursive"
	.align 2
.LC25:
	.string	"silent"
	.align 2
.LC24:
	.string	"quiet"
	.align 2
.LC23:
	.string	"name"
	.align 2
.LC22:
	.string	"no-name"
	.align 2
.LC21:
	.string	"license"
	.align 2
.LC20:
	.string	"list"
	.align 2
.LC19:
	.string	"help"
	.align 2
.LC18:
	.string	"force"
	.align 2
.LC17:
	.string	"uncompress"
	.align 2
.LC16:
	.string	"decompress"
	.align 2
.LC15:
	.string	"stdout"
	.align 2
.LC14:
	.string	"to-stdout"
	.align 2
.LC13:
	.string	"ascii"
	.size	 longopts,384
	.globl work
	.section ".sdata","aw"
	.align 2
	.type	 work,@object
	.size	 work,4
work:
	.long zip
	.section	".rodata"
	.align 2
.LC36:
	.string	"usage: %s [-%scdfhlLnN%stvV19] [-S suffix] [file ...]\n"
	.section ".text"
	.align 2
	.type	 usage,@function
usage:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC36@ha
	addi 4,4,.LC36@l
	lwz 5,progname@l(11)
	addis 6,0,.LC5@ha
	addi 6,6,.LC5@l
	mr 7,6
	crxor 6,6,6
	bl fprintf
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe1:
	.size	 usage,.Lfe1-usage
	.section ".data"
	.align 2
	.type	 help_msg.4,@object
help_msg.4:
	.long .LC37
	.long .LC38
	.long .LC39
	.long .LC40
	.long .LC41
	.long .LC42
	.long .LC43
	.long .LC44
	.long .LC45
	.long .LC46
	.long .LC47
	.long .LC48
	.long .LC49
	.long .LC50
	.long .LC51
	.long .LC52
	.long 0
	.section	".rodata"
	.align 2
.LC52:
	.string	" file...          files to (de)compress. If none given, use standard input."
	.align 2
.LC51:
	.string	" -9 --best        compress better"
	.align 2
.LC50:
	.string	" -1 --fast        compress faster"
	.align 2
.LC49:
	.string	" -V --version     display version number"
	.align 2
.LC48:
	.string	" -v --verbose     verbose mode"
	.align 2
.LC47:
	.string	" -t --test        test compressed file integrity"
	.align 2
.LC46:
	.string	" -S .suf  --suffix .suf     use suffix .suf on compressed files"
	.align 2
.LC45:
	.string	" -q --quiet       suppress all warnings"
	.align 2
.LC44:
	.string	" -N --name        save or restore the original name and time stamp"
	.align 2
.LC43:
	.string	" -n --no-name     do not save or restore the original name and time stamp"
	.align 2
.LC42:
	.string	" -L --license     display software license"
	.align 2
.LC41:
	.string	" -l --list        list compressed file contents"
	.align 2
.LC40:
	.string	" -h --help        give this help"
	.align 2
.LC39:
	.string	" -f --force       force overwrite of output file and compress links"
	.align 2
.LC38:
	.string	" -d --decompress  decompress"
	.align 2
.LC37:
	.string	" -c --stdout      write on standard output, keep original files unchanged"
	.align 2
.LC53:
	.string	"%s %s (%s)\n"
	.align 2
.LC54:
	.string	"1.2.4"
	.align 2
.LC55:
	.string	"18 Aug 93"
	.align 2
.LC56:
	.string	"%s\n"
	.section ".text"
	.align 2
	.type	 help,@function
help:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 29,0,help_msg.4@ha
	addi 31,29,help_msg.4@l
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC53@ha
	addi 4,4,.LC53@l
	lwz 5,progname@l(11)
	addis 6,0,.LC54@ha
	addi 6,6,.LC54@l
	addis 7,0,.LC55@ha
	addi 7,7,.LC55@l
	crxor 6,6,6
	bl fprintf
	bl usage
	lwz 0,help_msg.4@l(29)
	cmpwi 1,0,0
	bc 12,6,.L4
	addis 30,0,_impure_ptr@ha
	addis 29,0,.LC56@ha
.L5:
	lwz 9,_impure_ptr@l(30)
	lwz 3,12(9)
	addi 4,29,.LC56@l
	lwz 5,0(31)
	crxor 6,6,6
	bl fprintf
	lwzu 0,4(31)
	cmpwi 1,0,0
	bc 4,6,.L5
.L4:
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe2:
	.size	 help,.Lfe2-help
	.align 2
	.type	 license,@function
license:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 29,0,license_msg@ha
	addi 31,29,license_msg@l
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC53@ha
	addi 4,4,.LC53@l
	lwz 5,progname@l(11)
	addis 6,0,.LC54@ha
	addi 6,6,.LC54@l
	addis 7,0,.LC55@ha
	addi 7,7,.LC55@l
	crxor 6,6,6
	bl fprintf
	lwz 0,license_msg@l(29)
	cmpwi 1,0,0
	bc 12,6,.L9
	addis 30,0,_impure_ptr@ha
	addis 29,0,.LC56@ha
.L10:
	lwz 9,_impure_ptr@l(30)
	lwz 3,12(9)
	addi 4,29,.LC56@l
	lwz 5,0(31)
	crxor 6,6,6
	bl fprintf
	lwzu 0,4(31)
	cmpwi 1,0,0
	bc 4,6,.L10
.L9:
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe3:
	.size	 license,.Lfe3-license
	.section	".rodata"
	.align 2
.LC57:
	.string	"Compilation options:\n%s %s "
	.align 2
.LC58:
	.string	"NO_DIR"
	.align 2
.LC59:
	.string	"HAVE_UNISTD_H "
	.align 2
.LC60:
	.string	"NO_MEMORY_H "
	.align 2
.LC61:
	.string	"\n"
	.section ".text"
	.align 2
	.type	 version,@function
version:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 29,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(29)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC53@ha
	addi 4,4,.LC53@l
	lwz 5,progname@l(11)
	addis 6,0,.LC54@ha
	addi 6,6,.LC54@l
	addis 7,0,.LC55@ha
	addi 7,7,.LC55@l
	crxor 6,6,6
	bl fprintf
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC57@ha
	addi 4,4,.LC57@l
	addis 5,0,.LC58@ha
	addi 5,5,.LC58@l
	addis 6,0,.LC5@ha
	addi 6,6,.LC5@l
	crxor 6,6,6
	bl fprintf
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC59@ha
	addi 4,4,.LC59@l
	crxor 6,6,6
	bl fprintf
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC60@ha
	addi 4,4,.LC60@l
	crxor 6,6,6
	bl fprintf
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC61@ha
	addi 4,4,.LC61@l
	crxor 6,6,6
	bl fprintf
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe4:
	.size	 version,.Lfe4-version
	.section	".rodata"
	.align 2
.LC62:
	.string	".exe"
	.align 2
.LC63:
	.string	"GZIP"
	.align 2
.LC64:
	.string	"un"
	.align 2
.LC65:
	.string	"gun"
	.align 2
.LC66:
	.string	"cat"
	.align 2
.LC67:
	.string	"gzcat"
	.align 2
.LC68:
	.string	".gz"
	.align 2
.LC69:
	.string	"ab:cdfhH?lLmMnNqrS:tvVZ123456789"
	.align 2
.LC70:
	.string	"%s: -r not supported on this system\n"
	.align 2
.LC71:
	.string	"%s: -Z not supported in this version\n"
	.align 2
.LC72:
	.string	"%s: option --ascii ignored on this system\n"
	.align 2
.LC73:
	.string	"%s: incorrect suffix '%s'\n"
	.section ".text"
	.align 2
	.globl main
	.type	 main,@function
main:
	stwu 1,-80(1)
	mflr 0
	stw 18,24(1)
	stw 19,28(1)
	stw 20,32(1)
	stw 21,36(1)
	stw 22,40(1)
	stw 23,44(1)
	stw 24,48(1)
	stw 25,52(1)
	stw 26,56(1)
	stw 27,60(1)
	stw 28,64(1)
	stw 29,68(1)
	stw 30,72(1)
	stw 31,76(1)
	stw 3,16(1)
	stw 4,12(1)
	stw 0,84(1)
	bl __eabi
	lwz 3,16(1)
	lwz 4,12(1)
	stw 3,8(1)
	stw 4,12(1)
	lwz 3,0(4)
	bl basename
	addis 28,0,progname@ha
	stw 3,progname@l(28)
	bl strlen
	mr 29,3
	cmpwi 1,29,4
	bc 4,5,.L14
	lwz 3,progname@l(28)
	add 3,29,3
	addi 3,3,-4
	addis 4,0,.LC62@ha
	addi 4,4,.LC62@l
	bl strcmp
	mr. 3,3
	bc 4,2,.L14
	lwz 9,progname@l(28)
	add 9,29,9
	stb 3,-4(9)
.L14:
	addi 3,1,8
	addi 4,1,12
	addis 5,0,.LC63@ha
	addi 5,5,.LC63@l
	bl add_envopt
	addis 9,0,env@ha
	stw 3,env@l(9)
	cmpwi 1,3,0
	bc 12,6,.L15
	addis 9,0,args@ha
	lwz 0,12(1)
	stw 0,args@l(9)
.L15:
	addis 29,0,foreground@ha
	li 3,2
	li 4,1
	bl signal
	xori 3,3,1
	addic 0,3,-1
	subfe 3,0,3
	stw 3,foreground@l(29)
	cmpwi 1,3,0
	bc 12,6,.L16
	li 3,2
	addis 4,0,abort_gzip@ha
	addi 4,4,abort_gzip@l
	bl signal
.L16:
	li 3,15
	li 4,1
	bl signal
	cmpwi 1,3,1
	bc 12,6,.L17
	li 3,15
	addis 4,0,abort_gzip@ha
	addi 4,4,abort_gzip@l
	bl signal
.L17:
	li 3,1
	mr 4,3
	bl signal
	cmpwi 1,3,1
	bc 12,6,.L18
	li 3,1
	addis 4,0,abort_gzip@ha
	addi 4,4,abort_gzip@l
	bl signal
.L18:
	addis 29,0,progname@ha
	lwz 3,progname@l(29)
	addis 4,0,.LC64@ha
	addi 4,4,.LC64@l
	li 5,2
	bl strncmp
	cmpwi 1,3,0
	bc 12,6,.L20
	lwz 3,progname@l(29)
	addis 4,0,.LC65@ha
	addi 4,4,.LC65@l
	li 5,3
	bl strncmp
	cmpwi 1,3,0
	bc 4,6,.L19
.L20:
	addis 9,0,decompress@ha
	li 0,1
	stw 0,decompress@l(9)
	b .L21
.L19:
	addis 29,0,progname@ha
	lwz 3,progname@l(29)
	addi 3,3,1
	addis 4,0,.LC66@ha
	addi 4,4,.LC66@l
	bl strcmp
	cmpwi 1,3,0
	bc 12,6,.L23
	lwz 3,progname@l(29)
	addis 4,0,.LC67@ha
	addi 4,4,.LC67@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L21
.L23:
	addis 11,0,decompress@ha
	addis 9,0,to_stdout@ha
	li 0,1
	stw 0,to_stdout@l(9)
	stw 0,decompress@l(11)
.L21:
	addis 28,0,z_suffix@ha
	addi 3,28,z_suffix@l
	addis 4,0,.LC68@ha
	addi 4,4,.LC68@l
	li 5,30
	bl strncpy
	addis 29,0,z_len@ha
	addi 3,28,z_suffix@l
	bl strlen
	stw 3,z_len@l(29)
	addis 9,0,.L59@ha
	addi 18,9,.L59@l
	addis 9,0,.L59@ha
	addi 19,9,.L59@l
	li 28,1
	addis 24,0,optarg@ha
	addis 25,0,to_stdout@ha
	addis 26,0,decompress@ha
	addis 20,0,list@ha
	addis 30,0,no_time@ha
	li 31,0
	addis 21,0,no_name@ha
	addis 22,0,quiet@ha
	addis 27,0,verbose@ha
	addis 23,0,progname@ha
.L24:
	lwz 3,8(1)
	lwz 4,12(1)
	addis 5,0,.LC69@ha
	addi 5,5,.LC69@l
	addis 6,0,longopts@ha
	addi 6,6,longopts@l
	li 7,0
	bl getopt_long
	cmpwi 1,3,-1
	bc 12,6,.L25
	addi 0,3,-49
	cmplwi 1,0,69
	bc 12,5,.L58
	slwi 0,0,2
	lwzx 0,18,0
	add 0,0,19
	mtctr 0
	bctr
	.section	".rodata"
	.align 3
	.align 2
.L59:
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L57-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L35-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L35-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L37-.L59
	.long .L39-.L59
	.long .L41-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L44-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L47-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L48-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L28-.L59
	.long .L29-.L59
	.long .L30-.L59
	.long .L31-.L59
	.long .L58-.L59
	.long .L32-.L59
	.long .L58-.L59
	.long .L35-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L36-.L59
	.long .L38-.L59
	.long .L40-.L59
	.long .L58-.L59
	.long .L58-.L59
	.long .L42-.L59
	.long .L43-.L59
	.long .L58-.L59
	.long .L45-.L59
	.long .L58-.L59
	.long .L46-.L59
	.section ".text"
.L28:
	addis 9,0,ascii@ha
	stw 28,ascii@l(9)
	b .L24
.L29:
	lwz 3,optarg@l(24)
	bl atoi
	addis 9,0,maxbits@ha
	stw 3,maxbits@l(9)
	b .L24
.L30:
	stw 28,to_stdout@l(25)
	b .L24
.L31:
	stw 28,decompress@l(26)
	b .L24
.L32:
	addis 9,0,force@ha
	lwz 0,force@l(9)
	addic 0,0,1
	stw 0,force@l(9)
	b .L24
.L35:
	bl help
	b .L77
.L36:
	stw 28,to_stdout@l(25)
	stw 28,decompress@l(26)
	stw 28,list@l(20)
	b .L24
.L37:
	bl license
	b .L77
.L38:
	stw 28,no_time@l(30)
	b .L24
.L39:
	stw 31,no_time@l(30)
	b .L24
.L40:
	stw 28,no_time@l(30)
	stw 28,no_name@l(21)
	b .L24
.L41:
	stw 31,no_time@l(30)
	stw 31,no_name@l(21)
	b .L24
.L42:
	stw 28,quiet@l(22)
	stw 31,verbose@l(27)
	b .L24
.L43:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC70@ha
	addi 4,4,.LC70@l
.L78:
	lwz 5,progname@l(23)
	crxor 6,6,6
	bl fprintf
	b .L58
.L44:
	addis 29,0,z_len@ha
	lwz 3,optarg@l(24)
	bl strlen
	stw 3,z_len@l(29)
	addis 3,0,z_suffix@ha
	addi 3,3,z_suffix@l
	lwz 4,optarg@l(24)
	bl strcpy
	b .L24
.L45:
	addis 9,0,test@ha
	stw 28,to_stdout@l(25)
	stw 28,decompress@l(26)
	stw 28,test@l(9)
	b .L24
.L46:
	lwz 0,verbose@l(27)
	addic 0,0,1
	stw 0,verbose@l(27)
	stw 31,quiet@l(22)
	b .L24
.L47:
	bl version
.L77:
	li 3,0
	bl do_exit
	b .L24
.L48:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC71@ha
	addi 4,4,.LC71@l
	b .L78
.L57:
	addis 9,0,level@ha
	addi 0,3,-48
	stw 0,level@l(9)
	b .L24
.L58:
	bl usage
	li 3,1
	bl do_exit
	b .L24
.L25:
	addis 11,0,no_time@ha
	lwz 0,no_time@l(11)
	cmpwi 1,0,0
	bc 4,4,.L60
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	stw 0,no_time@l(11)
.L60:
	addis 11,0,no_name@ha
	lwz 0,no_name@l(11)
	cmpwi 1,0,0
	bc 4,4,.L61
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	stw 0,no_name@l(11)
.L61:
	addis 9,0,optind@ha
	lwz 11,8(1)
	lwz 0,optind@l(9)
	subf 31,0,11
	addis 9,0,ascii@ha
	lwz 0,ascii@l(9)
	cmpwi 1,0,0
	bc 12,6,.L62
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L62
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC72@ha
	addi 4,4,.LC72@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
.L62:
	addis 9,0,z_len@ha
	lwz 0,z_len@l(9)
	cmpwi 1,0,0
	bc 4,6,.L65
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L64
.L65:
	addis 9,0,z_len@ha
	lwz 0,z_len@l(9)
	cmpwi 1,0,30
	bc 4,5,.L63
.L64:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	addis 10,0,optarg@ha
	lwz 3,12(9)
	addis 4,0,.LC73@ha
	addi 4,4,.LC73@l
	lwz 5,progname@l(11)
	lwz 6,optarg@l(10)
	crxor 6,6,6
	bl fprintf
	li 3,1
	bl do_exit
.L63:
	addis 9,0,do_lzw@ha
	lwz 0,do_lzw@l(9)
	cmpwi 1,0,0
	bc 12,6,.L66
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 4,6,.L66
	addis 11,0,work@ha
	addis 9,0,lzw@ha
	addi 9,9,lzw@l
	stw 9,work@l(11)
.L66:
	cmpwi 1,31,0
	bc 12,6,.L67
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L68
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L68
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
.L68:
	addis 9,0,optind@ha
	lwz 9,optind@l(9)
	lwz 0,8(1)
	cmpw 1,9,0
	bc 4,4,.L74
	addis 29,0,optind@ha
.L72:
	lwz 0,optind@l(29)
	lwz 11,12(1)
	slwi 9,0,2
	addic 0,0,1
	stw 0,optind@l(29)
	lwzx 3,9,11
	bl treat_file
	lwz 9,optind@l(29)
	lwz 0,8(1)
	cmpw 1,9,0
	bc 12,4,.L72
	b .L74
.L67:
	bl treat_stdin
.L74:
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 12,6,.L75
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	subfic 9,0,0
	adde 0,9,0
	cmpwi 1,31,1
	mfcr 9
	rlwinm 9,9,6,1
	and. 11,0,9
	bc 12,2,.L75
	li 3,-1
	mr 4,3
	bl do_list
.L75:
	addis 29,0,exit_code@ha
	lwz 3,exit_code@l(29)
	bl do_exit
	lwz 3,exit_code@l(29)
	lwz 0,84(1)
	mtlr 0
	lwz 18,24(1)
	lwz 19,28(1)
	lwz 20,32(1)
	lwz 21,36(1)
	lwz 22,40(1)
	lwz 23,44(1)
	lwz 24,48(1)
	lwz 25,52(1)
	lwz 26,56(1)
	lwz 27,60(1)
	lwz 28,64(1)
	lwz 29,68(1)
	lwz 30,72(1)
	lwz 31,76(1)
	addi 1,1,80
	blr
.Lfe5:
	.size	 main,.Lfe5-main
	.section	".rodata"
	.align 2
.LC74:
	.string	"%s: compressed data not %s a terminal. Use -f to force %scompression.\n"
	.align 2
.LC75:
	.string	"read from"
	.align 2
.LC76:
	.string	"written to"
	.align 2
.LC77:
	.string	"de"
	.align 2
.LC78:
	.string	"For help, type: %s -h\n"
	.align 2
.LC79:
	.string	"stdin"
	.align 2
.LC80:
	.string	"fstat(stdin)"
	.align 2
.LC81:
	.string	" OK\n"
	.section ".text"
	.align 2
	.type	 treat_stdin,@function
treat_stdin:
	stwu 1,-48(1)
	mflr 0
	stw 22,8(1)
	stw 23,12(1)
	stw 24,16(1)
	stw 25,20(1)
	stw 26,24(1)
	stw 27,28(1)
	stw 28,32(1)
	stw 29,36(1)
	stw 30,40(1)
	stw 31,44(1)
	stw 0,52(1)
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 4,6,.L80
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L80
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L81
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,4(9)
	b .L82
.L81:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,8(9)
.L82:
	bl fileno
	bl isatty
	cmpwi 1,3,0
	bc 12,6,.L80
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,progname@ha
	addi 5,9,progname@l
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L83
	addis 9,0,.LC75@ha
	addi 6,9,.LC75@l
	b .L84
.L83:
	addis 9,0,.LC76@ha
	addi 6,9,.LC76@l
.L84:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L85
	addis 9,0,.LC77@ha
	addi 7,9,.LC77@l
	b .L86
.L85:
	addis 9,0,.LC5@ha
	addi 7,9,.LC5@l
.L86:
	lwz 3,12(11)
	addis 4,0,.LC74@ha
	addi 4,4,.LC74@l
	lwz 5,0(5)
	crxor 6,6,6
	bl fprintf
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC78@ha
	addi 4,4,.LC78@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
	li 3,1
	bl do_exit
.L80:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L89
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L89
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
.L89:
	addis 10,0,ifname@ha
	addi 8,10,ifname@l
	addis 9,0,.LC79@ha
	addi 11,9,.LC79@l
	lwz 0,.LC79@l(9)
	lhz 9,4(11)
	stw 0,ifname@l(10)
	sth 9,4(8)
	addis 10,0,ofname@ha
	addi 8,10,ofname@l
	addis 9,0,.LC15@ha
	addi 11,9,.LC15@l
	lwz 0,.LC15@l(9)
	lhz 9,4(11)
	lbz 11,6(11)
	stw 0,ofname@l(10)
	sth 9,4(8)
	stb 11,6(8)
	addis 9,0,time_stamp@ha
	li 0,0
	stw 0,time_stamp@l(9)
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L92
	addis 9,0,no_time@ha
	lwz 0,no_time@l(9)
	cmpwi 1,0,0
	bc 4,6,.L91
.L92:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,4(9)
	bl fileno
	addis 4,0,istat@ha
	addi 4,4,istat@l
	bl fstat
	cmpwi 1,3,0
	bc 12,6,.L93
	addis 3,0,.LC80@ha
	addi 3,3,.LC80@l
	bl error
.L93:
	addis 9,0,time_stamp@ha
	addis 11,0,istat+28@ha
	lwz 0,istat+28@l(11)
	stw 0,time_stamp@l(9)
.L91:
	addis 9,0,ifile_size@ha
	li 0,-1
	stw 0,ifile_size@l(9)
	bl clear_bufs
	addis 9,0,to_stdout@ha
	li 0,1
	stw 0,to_stdout@l(9)
	addis 9,0,part_nb@ha
	li 0,0
	stw 0,part_nb@l(9)
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L94
	addis 9,0,ifd@ha
	lwz 3,ifd@l(9)
	bl get_method
	addis 9,0,method@ha
	stw 3,method@l(9)
	cmpwi 1,3,0
	bc 4,4,.L94
	addis 9,0,exit_code@ha
	lwz 3,exit_code@l(9)
	bl do_exit
.L94:
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 12,6,.L96
	addis 9,0,ifd@ha
	addis 11,0,method@ha
	lwz 3,ifd@l(9)
	lwz 4,method@l(11)
	bl do_list
	b .L79
.L96:
	addis 22,0,work@ha
	addis 31,0,_impure_ptr@ha
	addis 23,0,decompress@ha
	addis 24,0,last_member@ha
	addis 25,0,inptr@ha
	addis 26,0,insize@ha
	addis 27,0,ifd@ha
	addis 28,0,method@ha
	addis 30,0,bytes_out@ha
.L97:
	lwz 9,_impure_ptr@l(31)
	lwz 3,4(9)
	bl fileno
	mr 29,3
	lwz 9,_impure_ptr@l(31)
	lwz 3,8(9)
	bl fileno
	mr 4,3
	lwz 0,work@l(22)
	mr 3,29
	mtlr 0
	blrl
	cmpwi 1,3,0
	bc 4,6,.L79
	lwz 0,decompress@l(23)
	cmpwi 1,0,0
	bc 12,6,.L98
	lwz 29,last_member@l(24)
	cmpwi 1,29,0
	bc 4,6,.L98
	lwz 0,inptr@l(25)
	lwz 9,insize@l(26)
	cmpw 1,0,9
	bc 12,6,.L98
	lwz 3,ifd@l(27)
	bl get_method
	stw 3,method@l(28)
	cmpwi 1,3,0
	bc 12,4,.L79
	stw 29,bytes_out@l(30)
	b .L97
.L98:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L79
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 12,6,.L105
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC81@ha
	addi 4,4,.LC81@l
	b .L108
.L105:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 4,6,.L79
	addis 10,0,bytes_in@ha
	addis 9,0,bytes_out@ha
	addis 11,0,header_bytes@ha
	lwz 3,bytes_out@l(9)
	lwz 0,header_bytes@l(11)
	subf 3,0,3
	lwz 4,bytes_in@l(10)
	addis 29,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(29)
	subf 3,3,4
	lwz 5,12(9)
	bl display_ratio
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC61@ha
	addi 4,4,.LC61@l
.L108:
	crxor 6,6,6
	bl fprintf
.L79:
	lwz 0,52(1)
	mtlr 0
	lwz 22,8(1)
	lwz 23,12(1)
	lwz 24,16(1)
	lwz 25,20(1)
	lwz 26,24(1)
	lwz 27,28(1)
	lwz 28,32(1)
	lwz 29,36(1)
	lwz 30,40(1)
	lwz 31,44(1)
	addi 1,1,48
	blr
.Lfe6:
	.size	 treat_stdin,.Lfe6-treat_stdin
	.section	".rodata"
	.align 2
.LC82:
	.string	"-"
	.align 2
.LC83:
	.string	"%s: %s is a directory -- ignored\n"
	.align 2
.LC84:
	.string	"%s: %s is not a directory or a regular file - ignored\n"
	.align 2
.LC85:
	.string	"%s: %s has %d other link%c -- unchanged\n"
	.align 2
.LC86:
	.string	"%s: "
	.align 2
.LC87:
	.string	"%s: %s compressed to %s\n"
	.align 2
.LC88:
	.string	"%s:\t%s"
	.align 2
.LC89:
	.string	"\t"
	.align 2
.LC90:
	.string	"\t\t"
	.align 2
.LC91:
	.string	" OK"
	.align 2
.LC92:
	.string	" -- replaced with %s"
	.section ".text"
	.align 2
	.type	 treat_file,@function
treat_file:
	stwu 1,-48(1)
	mflr 0
	stw 22,8(1)
	stw 23,12(1)
	stw 24,16(1)
	stw 25,20(1)
	stw 26,24(1)
	stw 27,28(1)
	stw 28,32(1)
	stw 29,36(1)
	stw 30,40(1)
	stw 31,44(1)
	stw 0,52(1)
	mr 29,3
	addis 4,0,.LC82@ha
	addi 4,4,.LC82@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L110
	addis 29,0,to_stdout@ha
	lwz 28,to_stdout@l(29)
	bl treat_stdin
	stw 28,to_stdout@l(29)
	b .L109
.L110:
	mr 3,29
	addis 29,0,istat@ha
	addi 4,29,istat@l
	bl get_istat
	cmpwi 1,3,0
	bc 4,6,.L109
	addi 9,29,istat@l
	lwz 0,4(9)
	rlwinm 0,0,0,16,19
	cmpwi 1,0,16384
	bc 4,6,.L112
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L119
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC83@ha
	addi 4,4,.LC83@l
	b .L163
.L112:
	addis 9,0,istat+4@ha
	lwz 0,istat+4@l(9)
	rlwinm 0,0,0,16,19
	li 9,0
	ori 9,9,32768
	cmpw 1,0,9
	bc 12,6,.L115
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L119
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC84@ha
	addi 4,4,.LC84@l
.L163:
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	b .L119
.L115:
	addis 9,0,istat+8@ha
	lhz 7,istat+8@l(9)
	cmplwi 1,7,1
	bc 4,5,.L118
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L118
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 4,6,.L118
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L119
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	subfic 8,7,2
	subfe 8,8,8
	andi. 8,8,115
	lwz 3,12(9)
	addis 4,0,.LC85@ha
	addi 4,4,.LC85@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	addi 7,7,-1
	ori 8,8,32
	crxor 6,6,6
	bl fprintf
.L119:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L109
	li 0,2
	stw 0,exit_code@l(9)
	b .L109
.L118:
	addis 11,0,ifile_size@ha
	addis 9,0,istat+16@ha
	lwz 0,istat+16@l(9)
	stw 0,ifile_size@l(11)
	addis 9,0,time_stamp@ha
	addi 11,9,time_stamp@l
	addis 9,0,no_time@ha
	lwz 0,no_time@l(9)
	cmpwi 1,0,0
	bc 12,6,.L125
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 12,6,.L123
.L125:
	addis 9,0,istat+28@ha
	lwz 0,istat+28@l(9)
	b .L124
.L123:
	li 0,0
.L124:
	stw 0,0(11)
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L126
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L126
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L126
	addis 10,0,ofname@ha
	addi 8,10,ofname@l
	addis 9,0,.LC15@ha
	addi 11,9,.LC15@l
	lwz 0,.LC15@l(9)
	lhz 9,4(11)
	lbz 11,6(11)
	stw 0,ofname@l(10)
	sth 9,4(8)
	stb 11,6(8)
	b .L127
.L126:
	bl make_ofname
	cmpwi 1,3,0
	bc 4,6,.L109
.L127:
	addis 29,0,ifname@ha
	addi 3,29,ifname@l
	li 4,0
	li 5,384
	crxor 6,6,6
	bl open
	addis 9,0,ifd@ha
	stw 3,ifd@l(9)
	cmpwi 1,3,-1
	bc 4,6,.L129
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
	addi 3,29,ifname@l
	bl perror
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	b .L109
.L129:
	bl clear_bufs
	addis 9,0,part_nb@ha
	li 0,0
	stw 0,part_nb@l(9)
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L130
	addis 29,0,ifd@ha
	lwz 3,ifd@l(29)
	bl get_method
	addis 9,0,method@ha
	stw 3,method@l(9)
	cmpwi 1,3,0
	bc 12,4,.L161
.L130:
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 12,6,.L132
	addis 29,0,ifd@ha
	addis 9,0,method@ha
	lwz 3,ifd@l(29)
	lwz 4,method@l(9)
	bl do_list
.L161:
	lwz 3,ifd@l(29)
	bl close
	b .L109
.L132:
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L133
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,8(9)
	bl fileno
	addis 9,0,ofd@ha
	stw 3,ofd@l(9)
	b .L134
.L133:
	bl create_outfile
	cmpwi 1,3,0
	bc 4,6,.L109
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 4,6,.L134
	addis 9,0,save_orig_name@ha
	lwz 0,save_orig_name@l(9)
	cmpwi 1,0,0
	bc 12,6,.L160
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 4,6,.L134
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L134
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC87@ha
	addi 4,4,.LC87@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	addis 7,0,ofname@ha
	addi 7,7,ofname@l
	crxor 6,6,6
	bl fprintf
.L134:
	addis 9,0,save_orig_name@ha
	lwz 0,save_orig_name@l(9)
	cmpwi 1,0,0
	bc 4,6,.L137
.L160:
	addis 11,0,save_orig_name@ha
	addis 9,0,no_name@ha
	lwz 0,no_name@l(9)
	subfic 9,0,0
	adde 0,9,0
	stw 0,save_orig_name@l(11)
.L137:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L138
	addis 9,0,_impure_ptr@ha
	lwz 28,_impure_ptr@l(9)
	addis 29,0,ifname@ha
	addi 3,29,ifname@l
	bl strlen
	cmpwi 1,3,14
	bc 12,5,.L139
	addi 3,29,ifname@l
	bl strlen
	cmpwi 1,3,6
	bc 4,5,.L141
	addis 9,0,.LC89@ha
	addi 6,9,.LC89@l
	b .L140
.L141:
	addis 9,0,.LC90@ha
	addi 6,9,.LC90@l
	b .L140
.L139:
	addis 9,0,.LC5@ha
	addi 6,9,.LC5@l
.L140:
	lwz 3,12(28)
	addis 4,0,.LC88@ha
	addi 4,4,.LC88@l
	addis 5,0,ifname@ha
	addi 5,5,ifname@l
	crxor 6,6,6
	bl fprintf
.L138:
	addis 22,0,work@ha
	addis 28,0,ifd@ha
	addis 23,0,ofd@ha
	addis 24,0,decompress@ha
	addis 25,0,last_member@ha
	addis 26,0,inptr@ha
	addis 27,0,insize@ha
	addis 30,0,method@ha
	addis 31,0,bytes_out@ha
.L143:
	lwz 0,work@l(22)
	lwz 3,ifd@l(28)
	lwz 4,ofd@l(23)
	mtlr 0
	blrl
	cmpwi 1,3,0
	bc 4,6,.L146
	lwz 0,decompress@l(24)
	cmpwi 1,0,0
	bc 12,6,.L144
	lwz 29,last_member@l(25)
	cmpwi 1,29,0
	bc 4,6,.L144
	lwz 0,inptr@l(26)
	lwz 9,insize@l(27)
	cmpw 1,0,9
	bc 12,6,.L144
	lwz 3,ifd@l(28)
	bl get_method
	stw 3,method@l(30)
	cmpwi 1,3,0
	bc 12,4,.L144
	stw 29,bytes_out@l(31)
	b .L143
.L146:
	addis 9,0,method@ha
	li 0,-1
	stw 0,method@l(9)
.L144:
	addis 9,0,ifd@ha
	lwz 3,ifd@l(9)
	bl close
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L150
	addis 9,0,ofd@ha
	lwz 3,ofd@l(9)
	bl close
	cmpwi 1,3,0
	bc 12,6,.L150
	bl write_error
.L150:
	addis 9,0,method@ha
	lwz 0,method@l(9)
	cmpwi 1,0,-1
	bc 4,6,.L151
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L109
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl unlink
	b .L109
.L151:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L153
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 12,6,.L154
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC91@ha
	addi 4,4,.LC91@l
	crxor 6,6,6
	bl fprintf
	b .L155
.L154:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L156
	addis 10,0,bytes_out@ha
	addis 9,0,bytes_in@ha
	addis 11,0,header_bytes@ha
	lwz 3,bytes_in@l(9)
	lwz 0,header_bytes@l(11)
	subf 3,0,3
	lwz 4,bytes_out@l(10)
	b .L162
.L156:
	addis 10,0,bytes_in@ha
	addis 9,0,bytes_out@ha
	addis 11,0,header_bytes@ha
	lwz 3,bytes_out@l(9)
	lwz 0,header_bytes@l(11)
	subf 3,0,3
	lwz 4,bytes_in@l(10)
.L162:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	subf 3,3,4
	lwz 5,12(9)
	bl display_ratio
.L155:
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L158
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L158
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC92@ha
	addi 4,4,.LC92@l
	addis 5,0,ofname@ha
	addi 5,5,ofname@l
	crxor 6,6,6
	bl fprintf
.L158:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC61@ha
	addi 4,4,.LC61@l
	crxor 6,6,6
	bl fprintf
.L153:
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L109
	addis 3,0,istat@ha
	addi 3,3,istat@l
	bl copy_stat
.L109:
	lwz 0,52(1)
	mtlr 0
	lwz 22,8(1)
	lwz 23,12(1)
	lwz 24,16(1)
	lwz 25,20(1)
	lwz 26,24(1)
	lwz 27,28(1)
	lwz 28,32(1)
	lwz 29,36(1)
	lwz 30,40(1)
	lwz 31,44(1)
	addi 1,1,48
	blr
.Lfe7:
	.size	 treat_file,.Lfe7-treat_file
	.section	".rodata"
	.align 2
.LC93:
	.string	"%s: %s: warning, name truncated\n"
	.section ".text"
	.align 2
	.type	 create_outfile,@function
create_outfile:
	stwu 1,-112(1)
	mflr 0
	stw 23,76(1)
	stw 24,80(1)
	stw 25,84(1)
	stw 26,88(1)
	stw 27,92(1)
	stw 28,96(1)
	stw 29,100(1)
	stw 30,104(1)
	stw 31,108(1)
	stw 0,116(1)
	li 23,2561
	addis 9,0,ascii@ha
	lwz 0,ascii@l(9)
	cmpwi 1,0,0
	addis 28,0,ifd@ha
	addis 24,0,remove_ofname@ha
	li 25,1
	addis 31,0,ofname@ha
	addis 30,0,ofd@ha
	addis 29,0,exit_code@ha
	addis 27,0,_impure_ptr@ha
	addis 26,0,progname@ha
.L166:
	bl check_ofname
	cmpwi 1,3,0
	bc 12,6,.L169
	lwz 3,ifd@l(28)
	bl close
	li 3,1
	b .L176
.L169:
	stw 25,remove_ofname@l(24)
	addi 3,31,ofname@l
	mr 4,23
	li 5,384
	crxor 6,6,6
	bl open
	stw 3,ofd@l(30)
	cmpwi 1,3,-1
	bc 4,6,.L170
	addi 3,31,ofname@l
	bl perror
	lwz 3,ifd@l(28)
	bl close
	b .L177
.L170:
	lwz 3,ofd@l(30)
	addi 4,1,8
	bl fstat
	cmpwi 1,3,0
	bc 12,6,.L171
	lwz 9,_impure_ptr@l(27)
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(26)
	crxor 6,6,6
	bl fprintf
	addi 3,31,ofname@l
	bl perror
	lwz 3,ifd@l(28)
	bl close
	lwz 3,ofd@l(30)
	bl close
	addi 3,31,ofname@l
	bl unlink
.L177:
	stw 25,exit_code@l(29)
	li 3,1
	b .L176
.L171:
	addi 3,31,ofname@l
	addi 4,1,8
	bl name_too_long
	cmpwi 1,3,0
	bc 4,6,.L172
.L178:
	li 3,0
	b .L176
.L172:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L173
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L174
	lwz 9,_impure_ptr@l(27)
	lwz 3,12(9)
	addis 4,0,.LC93@ha
	addi 4,4,.LC93@l
	lwz 5,progname@l(26)
	addi 6,31,ofname@l
	crxor 6,6,6
	bl fprintf
.L174:
	lwz 0,exit_code@l(29)
	cmpwi 1,0,0
	bc 4,6,.L178
	li 0,2
	stw 0,exit_code@l(29)
	b .L178
.L173:
	lwz 3,ofd@l(30)
	bl close
	addi 3,31,ofname@l
	bl unlink
	addi 3,31,ofname@l
	bl shorten_name
	b .L166
.L176:
	lwz 0,116(1)
	mtlr 0
	lwz 23,76(1)
	lwz 24,80(1)
	lwz 25,84(1)
	lwz 26,88(1)
	lwz 27,92(1)
	lwz 28,96(1)
	lwz 29,100(1)
	lwz 30,104(1)
	lwz 31,108(1)
	addi 1,1,112
	blr
.Lfe8:
	.size	 create_outfile,.Lfe8-create_outfile
	.align 2
	.type	 do_stat,@function
do_stat:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	mr 31,3
	mr 30,4
	bl __errno
	li 0,0
	stw 0,0(3)
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 4,6,.L180
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 4,6,.L180
	mr 3,31
	mr 4,30
	crxor 6,6,6
	bl lstat
	b .L181
.L180:
	mr 3,31
	mr 4,30
	bl stat
.L181:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe9:
	.size	 do_stat,.Lfe9-do_stat
	.section ".data"
	.align 2
	.type	 known_suffixes.21,@object
known_suffixes.21:
	.long z_suffix
	.long .LC68
	.long .LC94
	.long .LC95
	.long .LC96
	.long .LC97
	.long .LC98
	.long .LC99
	.long 0
	.section	".rodata"
	.align 2
.LC99:
	.string	"_z"
	.align 2
.LC98:
	.string	"-z"
	.align 2
.LC97:
	.string	"-gz"
	.align 2
.LC96:
	.string	".tgz"
	.align 2
.LC95:
	.string	".taz"
	.align 2
.LC94:
	.string	".z"
	.align 2
.LC100:
	.string	"z"
	.section ".text"
	.align 2
	.type	 get_suffix,@function
get_suffix:
	stwu 1,-80(1)
	mflr 0
	stw 25,52(1)
	stw 26,56(1)
	stw 27,60(1)
	stw 28,64(1)
	stw 29,68(1)
	stw 30,72(1)
	stw 31,76(1)
	stw 0,84(1)
	mr 26,3
	addis 9,0,known_suffixes.21@ha
	addi 30,9,known_suffixes.21@l
	addis 3,0,z_suffix@ha
	addi 3,3,z_suffix@l
	addis 4,0,.LC100@ha
	addi 4,4,.LC100@l
	bl strcmp
	srawi 9,3,31
	xor 0,9,3
	subf 0,0,9
	srawi 0,0,31
	addi 9,30,4
	and 11,0,30
	andc 0,9,0
	or 30,11,0
	mr 3,26
	bl strlen
	mr 27,3
	cmpwi 1,27,32
	bc 12,5,.L184
	addi 3,1,8
	mr 4,26
	b .L193
.L192:
	add 3,26,27
	subf 3,31,3
	b .L191
.L184:
	add 4,26,27
	addi 3,1,8
	addi 4,4,-32
.L193:
	bl strcpy
	addi 3,1,8
	bl strlwr
	addi 3,1,8
	bl strlen
	mr 29,3
	addi 28,1,8
	add 25,28,29
.L186:
	lwz 3,0(30)
	bl strlen
	mr 31,3
	cmpw 1,29,31
	bc 4,5,.L188
	subf 0,31,29
	addic 0,0,-1
	lbzx 0,28,0
	cmpwi 1,0,47
	bc 12,6,.L188
	subf 3,31,25
	lwz 4,0(30)
	bl strcmp
	cmpwi 1,3,0
	bc 12,6,.L192
.L188:
	lwzu 0,4(30)
	cmpwi 1,0,0
	bc 4,6,.L186
	li 3,0
.L191:
	lwz 0,84(1)
	mtlr 0
	lwz 25,52(1)
	lwz 26,56(1)
	lwz 27,60(1)
	lwz 28,64(1)
	lwz 29,68(1)
	lwz 30,72(1)
	lwz 31,76(1)
	addi 1,1,80
	blr
.Lfe10:
	.size	 get_suffix,.Lfe10-get_suffix
	.section ".data"
	.align 2
	.type	 suffixes.24,@object
suffixes.24:
	.long z_suffix
	.long .LC68
	.long .LC94
	.long .LC98
	.long .LC101
	.long 0
	.section	".rodata"
	.align 2
.LC101:
	.string	".Z"
	.section ".text"
	.align 2
	.type	 get_istat,@function
get_istat:
	stwu 1,-48(1)
	mflr 0
	stw 23,12(1)
	stw 24,16(1)
	stw 25,20(1)
	stw 26,24(1)
	stw 27,28(1)
	stw 28,32(1)
	stw 29,36(1)
	stw 30,40(1)
	stw 31,44(1)
	stw 0,52(1)
	mr 0,3
	mr 28,4
	addis 9,0,suffixes.24@ha
	addi 31,9,suffixes.24@l
	addis 29,0,ifname@ha
	addi 3,29,ifname@l
	mr 4,0
	bl strcpy
	addi 3,29,ifname@l
	mr 4,28
	bl do_stat
	cmpwi 1,3,0
	bc 12,6,.L203
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L197
	bl __errno
	lwz 0,0(3)
	cmpwi 1,0,2
	bc 12,6,.L196
.L197:
	addis 3,0,ifname@ha
	addi 3,3,ifname@l
	b .L206
.L196:
	addis 30,0,ifname@ha
	addi 3,30,ifname@l
	bl get_suffix
	cmpwi 1,3,0
	bc 12,6,.L198
	addi 3,30,ifname@l
.L206:
	bl perror
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	li 3,1
	b .L205
.L198:
	addis 3,0,ifname@ha
	addi 3,3,ifname@l
	bl strlen
	mr 27,3
	addis 3,0,z_suffix@ha
	addi 3,3,z_suffix@l
	addis 4,0,.LC68@ha
	addi 4,4,.LC68@l
	bl strcmp
	srawi 0,3,31
	xor 11,0,3
	subf 11,11,0
	srawi 11,11,31
	addi 0,31,4
	and 9,31,11
	andc 0,0,11
	or 31,9,0
	addis 30,0,ifname@ha
	mr 9,30
	addi 29,9,ifname@l
	li 26,0
	addis 25,0,z_suffix@ha
	addis 23,0,exit_code@ha
	li 24,1
.L200:
	addi 3,30,ifname@l
	lwz 4,0(31)
	bl strcat
	addi 3,30,ifname@l
	mr 4,28
	bl do_stat
	cmpwi 1,3,0
	bc 12,6,.L203
	stbx 26,29,27
	lwzu 0,4(31)
	cmpwi 1,0,0
	bc 4,6,.L200
	mr 3,29
	addi 4,25,z_suffix@l
	bl strcat
	mr 3,29
	bl perror
	stw 24,exit_code@l(23)
	li 3,1
	b .L205
.L203:
	li 3,0
.L205:
	lwz 0,52(1)
	mtlr 0
	lwz 23,12(1)
	lwz 24,16(1)
	lwz 25,20(1)
	lwz 26,24(1)
	lwz 27,28(1)
	lwz 28,32(1)
	lwz 29,36(1)
	lwz 30,40(1)
	lwz 31,44(1)
	addi 1,1,48
	blr
.Lfe11:
	.size	 get_istat,.Lfe11-get_istat
	.section	".rodata"
	.align 2
.LC102:
	.string	"%s: %s: unknown suffix -- ignored\n"
	.align 2
.LC103:
	.string	".tar"
	.align 2
.LC104:
	.string	"%s: %s already has %s suffix -- unchanged\n"
	.section ".text"
	.align 2
	.type	 make_ofname,@function
make_ofname:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 29,0,ofname@ha
	addi 3,29,ofname@l
	addis 4,0,ifname@ha
	addi 4,4,ifname@l
	bl strcpy
	addi 3,29,ofname@l
	bl get_suffix
	mr 29,3
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L208
	cmpwi 1,29,0
	bc 4,6,.L209
	addis 9,0,recursive@ha
	lwz 0,recursive@l(9)
	cmpwi 1,0,0
	bc 4,6,.L210
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L219
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L219
.L210:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 4,6,.L213
	addis 9,0,recursive@ha
	lwz 0,recursive@l(9)
	cmpwi 1,0,0
	bc 4,6,.L223
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L223
.L213:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L221
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC102@ha
	addi 4,4,.LC102@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	b .L221
.L209:
	mr 3,29
	bl strlwr
	mr 3,29
	addis 4,0,.LC96@ha
	addi 4,4,.LC96@l
	bl strcmp
	cmpwi 1,3,0
	bc 12,6,.L217
	mr 3,29
	addis 4,0,.LC95@ha
	addi 4,4,.LC95@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L216
.L217:
	addis 9,0,.LC103@ha
	addi 11,9,.LC103@l
	lwz 0,.LC103@l(9)
	lbz 9,4(11)
	stw 0,0(29)
	stb 9,4(29)
	b .L219
.L216:
	li 0,0
	stb 0,0(29)
	b .L219
.L208:
	cmpwi 1,29,0
	bc 12,6,.L220
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 4,6,.L222
	addis 9,0,recursive@ha
	lwz 0,recursive@l(9)
	cmpwi 1,0,0
	bc 4,6,.L221
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L221
.L222:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC104@ha
	addi 4,4,.LC104@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	mr 7,29
	crxor 6,6,6
	bl fprintf
.L221:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L223
	li 0,2
	stw 0,exit_code@l(9)
.L223:
	li 3,2
	b .L225
.L220:
	addis 9,0,save_orig_name@ha
	li 0,0
	stw 0,save_orig_name@l(9)
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	addis 4,0,z_suffix@ha
	addi 4,4,z_suffix@l
	bl strcat
.L219:
	li 3,0
.L225:
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe12:
	.size	 make_ofname,.Lfe12-make_ofname
	.section	".rodata"
	.align 2
.LC105:
	.string	"\037\213"
	.align 2
.LC106:
	.string	"\037\236"
	.align 2
.LC107:
	.string	"%s: %s: unknown method %d -- get newer version of gzip\n"
	.align 2
.LC108:
	.string	"%s: %s is encrypted -- get newer version of gzip\n"
	.align 2
.LC109:
	.string	"%s: %s is a a multi-part gzip file -- get newer version of gzip\n"
	.align 2
.LC110:
	.string	"%s: %s has flags 0x%x -- get newer version of gzip\n"
	.align 2
.LC111:
	.string	"%s: %s: part number %u\n"
	.align 2
.LC112:
	.string	"%s: %s: extra field of %u bytes ignored\n"
	.align 2
.LC113:
	.string	"corrupted input -- file name too large"
	.align 2
.LC114:
	.string	"PK\003\004"
	.align 2
.LC115:
	.string	"\037\036"
	.align 2
.LC116:
	.string	"\037\235"
	.align 2
.LC117:
	.string	"\037\240"
	.align 2
.LC118:
	.string	"\n%s: %s: not in gzip format\n"
	.align 2
.LC119:
	.string	"\n%s: %s: decompression OK, trailing garbage ignored\n"
	.section ".text"
	.align 2
	.type	 get_method,@function
get_method:
	stwu 1,-48(1)
	mflr 0
	stw 24,16(1)
	stw 25,20(1)
	stw 26,24(1)
	stw 27,28(1)
	stw 28,32(1)
	stw 29,36(1)
	stw 30,40(1)
	stw 31,44(1)
	stw 0,52(1)
	mr 29,3
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 12,6,.L227
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L227
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L228
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L229
.L228:
	li 3,1
	bl fill_inbuf
.L229:
	rlwinm 9,3,0,0xff
	slwi 9,9,8
	lbz 0,9(1)
	or 0,0,9
	sth 0,8(1)
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 12,4,.L331
	li 3,1
	b .L332
.L227:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L233
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L234
.L233:
	li 3,0
	bl fill_inbuf
.L234:
	rlwinm 9,3,0,0xff
	slwi 9,9,8
	lbz 0,9(1)
	or 0,0,9
	sth 0,8(1)
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L235
.L331:
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L236
.L235:
	li 3,0
.L332:
	bl fill_inbuf
.L236:
	rlwinm 9,3,0,0xff
	lhz 0,8(1)
	rlwinm 0,0,0,16,23
	or 0,0,9
	sth 0,8(1)
	addis 9,0,method@ha
	li 0,-1
	stw 0,method@l(9)
	addis 9,0,part_nb@ha
	lwz 0,part_nb@l(9)
	addic 0,0,1
	stw 0,part_nb@l(9)
	addis 9,0,header_bytes@ha
	li 0,0
	stw 0,header_bytes@l(9)
	addis 9,0,last_member@ha
	stw 0,last_member@l(9)
	addi 3,1,8
	addis 4,0,.LC105@ha
	addi 4,4,.LC105@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 12,6,.L238
	addi 3,1,8
	addis 4,0,.LC106@ha
	addi 4,4,.LC106@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 4,6,.L237
.L238:
	addis 9,0,method@ha
	addi 31,9,method@l
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L239
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L240
.L239:
	li 3,0
	bl fill_inbuf
.L240:
	stw 3,0(31)
	addis 9,0,method@ha
	lwz 7,method@l(9)
	cmpwi 1,7,8
	bc 12,6,.L241
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC107@ha
	addi 4,4,.LC107@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	b .L325
.L241:
	addis 11,0,work@ha
	addis 9,0,unzip@ha
	addi 9,9,unzip@l
	stw 9,work@l(11)
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L242
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 7,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L243
.L242:
	li 3,0
	bl fill_inbuf
	rlwinm 7,3,0,0xff
.L243:
	mr 28,7
	andi. 0,28,32
	bc 12,2,.L244
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC108@ha
	addi 4,4,.LC108@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
.L325:
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
.L330:
	li 3,-1
	b .L324
.L244:
	andi. 0,28,2
	bc 12,2,.L245
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC109@ha
	addi 4,4,.LC109@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,1
	bc 4,5,.L330
.L245:
	andi. 0,28,192
	bc 12,2,.L247
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC110@ha
	addi 4,4,.LC110@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	mr 7,28
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,1
	bc 4,5,.L330
.L247:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L249
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 31,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L250
.L249:
	li 3,0
	bl fill_inbuf
	mr 31,3
.L250:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L251
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 0,9,11
	slwi 0,0,8
	or 31,31,0
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L252
.L251:
	li 3,0
	bl fill_inbuf
	slwi 3,3,8
	or 31,31,3
.L252:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L253
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 0,9,11
	slwi 0,0,16
	or 31,31,0
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L254
.L253:
	li 3,0
	bl fill_inbuf
	slwi 3,3,16
	or 31,31,3
.L254:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L255
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 0,9,11
	slwi 0,0,24
	or 31,31,0
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L256
.L255:
	li 3,0
	bl fill_inbuf
	slwi 3,3,24
	or 31,31,3
.L256:
	cmpwi 1,31,0
	bc 12,6,.L257
	addis 9,0,no_time@ha
	lwz 0,no_time@l(9)
	cmpwi 1,0,0
	bc 4,6,.L257
	addis 9,0,time_stamp@ha
	stw 31,time_stamp@l(9)
.L257:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L258
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L259
.L258:
	li 3,0
	bl fill_inbuf
.L259:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L260
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L261
.L260:
	li 3,0
	bl fill_inbuf
.L261:
	andi. 0,28,2
	bc 12,2,.L262
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L263
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 31,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L264
.L263:
	li 3,0
	bl fill_inbuf
	mr 31,3
.L264:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L265
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 0,9,11
	slwi 0,0,8
	or 31,31,0
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L266
.L265:
	li 3,0
	bl fill_inbuf
	slwi 3,3,8
	or 31,31,3
.L266:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L262
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC111@ha
	addi 4,4,.LC111@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	mr 7,31
	crxor 6,6,6
	bl fprintf
.L262:
	andi. 0,28,4
	bc 12,2,.L268
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L269
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 31,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L270
.L269:
	li 3,0
	bl fill_inbuf
	mr 31,3
.L270:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L271
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 0,9,11
	slwi 0,0,8
	or 31,31,0
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L272
.L271:
	li 3,0
	bl fill_inbuf
	slwi 3,3,8
	or 31,31,3
.L272:
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L273
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC112@ha
	addi 4,4,.LC112@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	mr 7,31
	crxor 6,6,6
	bl fprintf
.L273:
	cmpwi 1,31,0
	addi 31,31,-1
	bc 12,6,.L268
	addis 30,0,inptr@ha
	addis 29,0,insize@ha
.L276:
	lwz 9,inptr@l(30)
	lwz 0,insize@l(29)
	cmplw 1,9,0
	bc 4,4,.L277
	addi 0,9,1
	stw 0,inptr@l(30)
	b .L274
.L277:
	li 3,0
	bl fill_inbuf
.L274:
	cmpwi 1,31,0
	addi 31,31,-1
	bc 4,6,.L276
.L268:
	andi. 0,28,8
	bc 12,2,.L280
	addis 9,0,no_name@ha
	lwz 0,no_name@l(9)
	cmpwi 1,0,0
	bc 4,6,.L282
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L283
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 12,6,.L282
.L283:
	addis 9,0,part_nb@ha
	lwz 0,part_nb@l(9)
	cmpwi 1,0,1
	bc 4,5,.L281
.L282:
	addis 31,0,inptr@ha
	addis 29,0,insize@ha
	addis 9,0,inbuf@ha
	addi 30,9,inbuf@l
.L284:
	lwz 9,inptr@l(31)
	lwz 0,insize@l(29)
	cmplw 1,9,0
	bc 4,4,.L287
	lbzx 3,30,9
	addi 0,9,1
	stw 0,inptr@l(31)
	b .L288
.L287:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
.L288:
	cmpwi 1,3,0
	bc 4,6,.L284
	b .L280
.L281:
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl basename
	mr 29,3
	mr 31,29
	addis 30,0,inptr@ha
	addis 24,0,insize@ha
	addis 9,0,inbuf@ha
	addi 25,9,inbuf@l
	addis 9,0,ofname+1024@ha
	addi 26,9,ofname+1024@l
	addis 27,0,.LC113@ha
.L291:
	lwz 9,inptr@l(30)
	lwz 0,insize@l(24)
	cmplw 1,9,0
	bc 4,4,.L294
	lbzx 3,25,9
	addi 0,9,1
	stw 0,inptr@l(30)
	b .L295
.L294:
	li 3,0
	bl fill_inbuf
.L295:
	stb 3,0(31)
	rlwinm 0,3,0,0xff
	addi 31,31,1
	cmpwi 1,0,0
	bc 12,6,.L292
	cmplw 1,31,26
	bc 12,4,.L291
	addi 3,27,.LC113@l
	bl error
	b .L291
.L292:
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L280
	cmpwi 1,29,0
.L280:
	andi. 0,28,16
	bc 12,2,.L300
	b .L301
.L304:
	li 3,0
	bl fill_inbuf
	cmpwi 1,3,0
	bc 12,6,.L300
.L301:
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L304
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 9,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	cmpwi 1,9,0
	bc 4,6,.L301
.L300:
	addis 9,0,part_nb@ha
	lwz 0,part_nb@l(9)
	cmpwi 1,0,1
	bc 4,6,.L308
	addis 11,0,header_bytes@ha
	addis 9,0,inptr@ha
	lwz 0,inptr@l(9)
	addic 0,0,8
	stw 0,header_bytes@l(11)
	b .L308
.L237:
	addi 3,1,8
	addis 30,0,.LC114@ha
	addi 4,30,.LC114@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 4,6,.L309
	addis 31,0,inptr@ha
	lwz 0,inptr@l(31)
	cmpwi 1,0,2
	bc 4,6,.L309
	addis 3,0,inbuf@ha
	addi 3,3,inbuf@l
	addi 4,30,.LC114@l
	li 5,4
	bl memcmp
	mr. 3,3
	bc 4,2,.L309
	stw 3,inptr@l(31)
	addis 11,0,work@ha
	addis 9,0,unzip@ha
	addi 9,9,unzip@l
	stw 9,work@l(11)
	mr 3,29
	bl check_zipfile
	cmpwi 1,3,0
	bc 4,6,.L330
	b .L328
.L309:
	addi 3,1,8
	addis 4,0,.LC115@ha
	addi 4,4,.LC115@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 4,6,.L312
	addis 11,0,work@ha
	addis 9,0,unpack@ha
	addi 9,9,unpack@l
	stw 9,work@l(11)
	addis 9,0,method@ha
	li 0,2
	stw 0,method@l(9)
	b .L308
.L312:
	addi 3,1,8
	addis 4,0,.LC116@ha
	addi 4,4,.LC116@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 4,6,.L314
	addis 11,0,work@ha
	addis 9,0,unlzw@ha
	addi 9,9,unlzw@l
	stw 9,work@l(11)
	addis 9,0,method@ha
	li 0,1
	stw 0,method@l(9)
	addis 9,0,last_member@ha
	b .L329
.L314:
	addi 3,1,8
	addis 4,0,.LC117@ha
	addi 4,4,.LC117@l
	li 5,2
	bl memcmp
	cmpwi 1,3,0
	bc 4,6,.L316
	addis 11,0,work@ha
	addis 9,0,unlzh@ha
	addi 9,9,unlzh@l
	stw 9,work@l(11)
	addis 9,0,method@ha
	li 0,3
	stw 0,method@l(9)
	b .L328
.L316:
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 12,6,.L308
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L308
	addis 9,0,list@ha
	lwz 0,list@l(9)
	cmpwi 1,0,0
	bc 4,6,.L308
	addis 9,0,method@ha
	stw 0,method@l(9)
	addis 11,0,work@ha
	addis 9,0,copy@ha
	addi 9,9,copy@l
	stw 9,work@l(11)
	addis 9,0,inptr@ha
	stw 0,inptr@l(9)
.L328:
	addis 9,0,last_member@ha
	li 0,1
.L329:
	stw 0,last_member@l(9)
.L308:
	addis 9,0,method@ha
	lwz 3,method@l(9)
	cmpwi 1,3,0
	bc 4,4,.L324
	addis 9,0,part_nb@ha
	lwz 31,part_nb@l(9)
	cmpwi 1,31,1
	bc 4,6,.L320
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC118@ha
	addi 4,4,.LC118@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	stw 31,exit_code@l(9)
	b .L330
.L320:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L322
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC119@ha
	addi 4,4,.LC119@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
.L322:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L323
	li 0,2
	stw 0,exit_code@l(9)
.L323:
	li 3,-2
.L324:
	lwz 0,52(1)
	mtlr 0
	lwz 24,16(1)
	lwz 25,20(1)
	lwz 26,24(1)
	lwz 27,28(1)
	lwz 28,32(1)
	lwz 29,36(1)
	lwz 30,40(1)
	lwz 31,44(1)
	addi 1,1,48
	blr
.Lfe13:
	.size	 get_method,.Lfe13-get_method
	.section ".sdata","aw"
	.align 2
	.type	 first_time.31,@object
	.size	 first_time.31,4
first_time.31:
	.long 1
	.section ".data"
	.align 2
	.type	 methods.32,@object
	.size	 methods.32,36
methods.32:
	.long .LC120
	.long .LC121
	.long .LC122
	.long .LC123
	.long .LC5
	.long .LC5
	.long .LC5
	.long .LC5
	.long .LC124
	.section	".rodata"
	.align 2
.LC124:
	.string	"defla"
	.align 2
.LC123:
	.string	"lzh  "
	.align 2
.LC122:
	.string	"pack "
	.align 2
.LC121:
	.string	"compr"
	.align 2
.LC120:
	.string	"store"
	.align 2
.LC125:
	.string	"method  crc     date  time  "
	.align 2
.LC126:
	.string	"compressed  uncompr. ratio uncompressed_name\n"
	.align 2
.LC127:
	.string	"                            %9lu %9lu "
	.align 2
.LC128:
	.string	"%9ld %9ld "
	.align 2
.LC129:
	.string	" (totals)\n"
	.align 2
.LC130:
	.string	"%5s %08lx %11s "
	.align 2
.LC131:
	.string	" %s\n"
	.section ".text"
	.align 2
	.type	 do_list,@function
do_list:
	stwu 1,-32(1)
	mflr 0
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 28,3
	mr 29,4
	addis 10,0,first_time.31@ha
	lwz 0,first_time.31@l(10)
	addic 9,0,-1
	subfe 0,9,0
	nor 9,29,29
	srwi 9,9,31
	and. 11,0,9
	bc 12,2,.L334
	li 0,0
	stw 0,first_time.31@l(10)
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L335
	addis 3,0,.LC125@ha
	addi 3,3,.LC125@l
	crxor 6,6,6
	bl printf
.L335:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L337
	addis 3,0,.LC126@ha
	addi 3,3,.LC126@l
	crxor 6,6,6
	bl printf
	b .L337
.L334:
	cmpwi 1,29,0
	bc 4,4,.L337
	addis 9,0,total_in@ha
	lwz 4,total_in@l(9)
	cmpwi 1,4,0
	bc 4,5,.L333
	addis 9,0,total_out@ha
	lwz 5,total_out@l(9)
	cmpwi 1,5,0
	bc 4,5,.L333
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L341
	addis 3,0,.LC127@ha
	addi 3,3,.LC127@l
	b .L354
.L341:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L342
	addis 9,0,total_in@ha
	addis 11,0,total_out@ha
	addis 3,0,.LC128@ha
	addi 3,3,.LC128@l
	lwz 4,total_in@l(9)
	lwz 5,total_out@l(11)
.L354:
	crxor 6,6,6
	bl printf
.L342:
	addis 10,0,total_out@ha
	addis 9,0,total_in@ha
	addis 11,0,header_bytes@ha
	lwz 3,total_in@l(9)
	lwz 0,header_bytes@l(11)
	subf 3,0,3
	lwz 4,total_out@l(10)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	subf 3,3,4
	lwz 5,8(9)
	bl display_ratio
	addis 3,0,.LC129@ha
	addi 3,3,.LC129@l
	crxor 6,6,6
	bl printf
	b .L333
.L337:
	li 31,-1
	addis 9,0,bytes_out@ha
	stw 31,bytes_out@l(9)
	addis 30,0,bytes_in@ha
	addis 9,0,ifile_size@ha
	lwz 0,ifile_size@l(9)
	stw 0,bytes_in@l(30)
	cmpwi 1,29,8
	bc 4,6,.L344
	addis 9,0,last_member@ha
	lwz 0,last_member@l(9)
	cmpwi 1,0,0
	bc 4,6,.L344
	mr 3,28
	li 4,-8
	li 5,2
	bl lseek
	stw 3,bytes_in@l(30)
	cmpw 1,3,31
	bc 12,6,.L344
	addi 0,3,8
	stw 0,bytes_in@l(30)
	mr 3,28
	addi 4,1,8
	li 5,8
	bl read
	cmpwi 1,3,8
	bc 12,6,.L346
	bl read_error
.L346:
	lbz 31,8(1)
	lbz 0,9(1)
	slwi 0,0,8
	or 31,31,0
	lbz 9,10(1)
	lbz 0,11(1)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 31,31,9
	addis 10,0,bytes_out@ha
	lbz 11,12(1)
	lbz 0,13(1)
	slwi 0,0,8
	or 11,11,0
	lbz 9,14(1)
	lbz 0,15(1)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 11,11,9
	stw 11,bytes_out@l(10)
.L344:
	addis 3,0,time_stamp@ha
	addi 3,3,time_stamp@l
	bl ctime
	addi 6,3,4
	li 0,0
	stb 0,12(6)
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,0
	bc 12,6,.L347
	addis 9,0,methods.32@ha
	addi 9,9,methods.32@l
	slwi 0,29,2
	addis 3,0,.LC130@ha
	addi 3,3,.LC130@l
	lwzx 4,9,0
	mr 5,31
	crxor 6,6,6
	bl printf
.L347:
	addis 31,0,bytes_in@ha
	addis 30,0,bytes_out@ha
	addis 3,0,.LC128@ha
	addi 3,3,.LC128@l
	lwz 4,bytes_in@l(31)
	lwz 5,bytes_out@l(30)
	crxor 6,6,6
	bl printf
	lwz 0,bytes_in@l(31)
	cmpwi 1,0,-1
	bc 4,6,.L348
	addis 9,0,total_in@ha
	stw 0,total_in@l(9)
	addis 9,0,header_bytes@ha
	li 0,0
	stw 0,header_bytes@l(9)
	stw 0,bytes_out@l(30)
	stw 0,bytes_in@l(31)
	b .L349
.L348:
	addis 10,0,total_in@ha
	lwz 11,total_in@l(10)
	cmpwi 1,11,0
	bc 12,4,.L349
	addis 9,0,bytes_in@ha
	lwz 0,bytes_in@l(9)
	add 0,11,0
	stw 0,total_in@l(10)
.L349:
	addis 10,0,bytes_out@ha
	lwz 0,bytes_out@l(10)
	cmpwi 1,0,-1
	bc 4,6,.L351
	addis 9,0,total_out@ha
	stw 0,total_out@l(9)
	addis 11,0,bytes_in@ha
	addis 9,0,header_bytes@ha
	li 0,0
	stw 0,header_bytes@l(9)
	stw 0,bytes_out@l(10)
	stw 0,bytes_in@l(11)
	b .L352
.L351:
	addis 10,0,total_out@ha
	lwz 11,total_out@l(10)
	cmpwi 1,11,0
	bc 12,4,.L352
	addis 9,0,bytes_out@ha
	lwz 0,bytes_out@l(9)
	add 0,11,0
	stw 0,total_out@l(10)
.L352:
	addis 10,0,bytes_out@ha
	addis 9,0,bytes_in@ha
	addis 11,0,header_bytes@ha
	lwz 3,bytes_in@l(9)
	lwz 0,header_bytes@l(11)
	subf 3,0,3
	lwz 4,bytes_out@l(10)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	subf 3,3,4
	lwz 5,8(9)
	bl display_ratio
	addis 3,0,.LC131@ha
	addi 3,3,.LC131@l
	addis 4,0,ofname@ha
	addi 4,4,ofname@l
	crxor 6,6,6
	bl printf
.L333:
	lwz 0,36(1)
	mtlr 0
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe14:
	.size	 do_list,.Lfe14-do_list
	.align 2
	.type	 same_file,@function
same_file:
	li 10,0
	lwz 0,0(3)
	lwz 9,0(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lwz 0,4(3)
	lwz 9,4(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lhz 0,10(3)
	lhz 9,10(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lhz 0,12(3)
	lhz 9,12(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lwz 0,16(3)
	lwz 9,16(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lwz 0,20(3)
	lwz 9,20(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lwz 0,28(3)
	lwz 9,28(4)
	cmpw 1,0,9
	bc 4,6,.L356
	lwz 0,36(3)
	lwz 9,36(4)
	xor 10,0,9
	subfic 11,10,0
	adde 10,11,10
.L356:
	mr 3,10
	blr
.Lfe15:
	.size	 same_file,.Lfe15-same_file
	.globl memcpy
	.align 2
	.type	 name_too_long,@function
name_too_long:
	stwu 1,-112(1)
	mflr 0
	stw 27,92(1)
	stw 28,96(1)
	stw 29,100(1)
	stw 30,104(1)
	stw 31,108(1)
	stw 0,116(1)
	mr 31,3
	mr 30,4
	bl strlen
	mr 28,3
	add 29,28,31
	lbz 27,-1(29)
	addi 3,1,24
	mr 4,30
	li 5,60
	crxor 6,6,6
	bl memcpy
	li 0,0
	stb 0,-1(29)
	li 29,0
	mr 3,31
	addi 4,1,24
	bl stat
	cmpw 1,3,29
	bc 4,6,.L358
	mr 3,30
	addi 4,1,24
	bl same_file
	addic 0,3,-1
	subfe 29,0,3
.L358:
	add 9,28,31
	stb 27,-1(9)
	mr 3,29
	lwz 0,116(1)
	mtlr 0
	lwz 27,92(1)
	lwz 28,96(1)
	lwz 29,100(1)
	lwz 30,104(1)
	lwz 31,108(1)
	addi 1,1,112
	blr
.Lfe16:
	.size	 name_too_long,.Lfe16-name_too_long
	.section	".rodata"
	.align 2
.LC132:
	.string	"name too short"
	.align 2
.LC133:
	.string	"can't recover suffix\n"
	.align 2
.LC134:
	.string	"."
	.align 2
.LC135:
	.string	"internal error in shorten_name"
	.section ".text"
	.align 2
	.type	 shorten_name,@function
shorten_name:
	stwu 1,-32(1)
	mflr 0
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 27,3
	li 30,0
	li 29,3
	bl strlen
	mr 28,3
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpw 1,0,30
	bc 12,6,.L360
	cmpwi 1,28,1
	bc 12,5,.L361
	addis 3,0,.LC132@ha
	addi 3,3,.LC132@l
	bl error
.L361:
	add 9,28,27
	li 0,0
	stb 0,-1(9)
	b .L359
.L360:
	mr 3,27
	bl get_suffix
	mr. 31,3
	bc 4,2,.L362
	addis 3,0,.LC133@ha
	addi 3,3,.LC133@l
	bl error
.L362:
	li 0,0
	stb 0,0(31)
	addis 9,0,save_orig_name@ha
	li 0,1
	stw 0,save_orig_name@l(9)
	cmpwi 1,28,4
	bc 4,5,.L363
	addi 28,31,-4
	mr 3,28
	addis 4,0,.LC103@ha
	addi 4,4,.LC103@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L363
	addis 9,0,.LC96@ha
	addi 11,9,.LC96@l
	lwz 0,.LC96@l(9)
	lbz 9,4(11)
	stw 0,-4(31)
	stb 9,4(28)
	b .L359
.L363:
	addis 28,0,.LC134@ha
.L364:
	mr 3,27
	li 4,47
	bl strrchr
	addic 9,3,-1
	subfe 9,9,9
	addi 3,3,1
	and 0,27,9
	andc 3,3,9
	or 31,0,3
	b .L386
.L371:
	mr 3,31
	addi 4,28,.LC134@l
	bl strcspn
	add 31,31,3
	cmpw 1,3,29
	cror 7,6,4
	mfcr 0
	rlwinm 0,0,8,1
	neg 0,0
	addi 9,31,-1
	and 11,30,0
	andc 3,9,0
	or 30,11,3
	lbz 0,0(31)
	addic 11,0,-1
	subfe 11,11,11
	addi 0,31,1
	and 9,31,11
	andc 0,0,11
	or 31,9,0
.L386:
	lbz 0,0(31)
	cmpwi 1,0,0
	bc 4,6,.L371
	cmpwi 1,30,0
	bc 4,6,.L378
	addic. 29,29,-1
	bc 4,2,.L364
	bc 12,6,.L377
.L378:
	lbz 0,1(30)
	stb 0,0(30)
	rlwinm 0,0,0,0xff
	addi 30,30,1
	cmpwi 1,0,0
	bc 4,6,.L378
	addi 30,30,-1
	b .L382
.L377:
	mr 3,27
	li 4,46
	bl strrchr
	mr. 30,3
	bc 4,2,.L383
	addis 3,0,.LC135@ha
	addi 3,3,.LC135@l
	bl error
.L383:
	lbz 0,1(30)
	neg 0,0
	srawi 0,0,31
	addi 9,30,-1
	and 11,30,0
	andc 3,9,0
	or 30,11,3
.L382:
	mr 3,30
	addis 4,0,z_suffix@ha
	addi 4,4,z_suffix@l
	bl strcpy
.L359:
	lwz 0,36(1)
	mtlr 0
	lwz 27,12(1)
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe17:
	.size	 shorten_name,.Lfe17-shorten_name
	.section	".rodata"
	.align 2
.LC136:
	.string	"%s: %s: cannot %scompress onto itself\n"
	.align 2
.LC137:
	.string	"%s: %s and %s are the same file\n"
	.align 2
.LC138:
	.string	"n"
	.align 2
.LC139:
	.string	"%s: %s already exists;"
	.align 2
.LC140:
	.string	" do you wish to overwrite (y or n)? "
	.align 2
.LC141:
	.string	"\tnot overwritten\n"
	.section ".text"
	.align 2
	.type	 check_ofname,@function
check_ofname:
	stwu 1,-168(1)
	mflr 0
	stw 29,156(1)
	stw 30,160(1)
	stw 31,164(1)
	stw 0,172(1)
	bl __errno
	li 0,0
	stw 0,0(3)
	addis 29,0,ofname@ha
.L388:
	addi 3,29,ofname@l
	addi 4,1,8
	bl stat
	cmpwi 1,3,0
	bc 12,6,.L389
	bl __errno
	lwz 0,0(3)
	cmpwi 1,0,91
	bc 4,6,.L411
	addi 3,29,ofname@l
	bl shorten_name
	b .L388
.L389:
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 4,6,.L392
	addis 29,0,ofname@ha
	addi 3,29,ofname@l
	addi 4,1,8
	bl name_too_long
	cmpwi 1,3,0
	bc 12,6,.L392
	addi 3,29,ofname@l
	bl shorten_name
	addi 3,29,ofname@l
	addi 4,1,8
	bl stat
	cmpwi 1,3,0
	li 3,0
	bc 4,6,.L406
.L392:
	addis 3,0,istat@ha
	addi 3,3,istat@l
	addi 4,1,8
	bl same_file
	cmpwi 1,3,0
	bc 12,6,.L394
	addis 3,0,ifname@ha
	addi 3,3,ifname@l
	addis 4,0,ofname@ha
	addi 4,4,ofname@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L395
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,progname@ha
	addi 5,9,progname@l
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L396
	addis 9,0,.LC77@ha
	addi 7,9,.LC77@l
	b .L397
.L396:
	addis 9,0,.LC5@ha
	addi 7,9,.LC5@l
.L397:
	lwz 3,12(11)
	addis 4,0,.LC136@ha
	addi 4,4,.LC136@l
	lwz 5,0(5)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	b .L408
.L395:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC137@ha
	addi 4,4,.LC137@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	addis 7,0,ofname@ha
	addi 7,7,ofname@l
.L408:
	crxor 6,6,6
	bl fprintf
	b .L409
.L394:
	addis 9,0,force@ha
	lwz 0,force@l(9)
	cmpwi 1,0,0
	bc 4,6,.L399
	addis 9,0,.LC138@ha
	lhz 0,.LC138@l(9)
	sth 0,72(1)
	addis 29,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(29)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC139@ha
	addi 4,4,.LC139@l
	lwz 5,progname@l(11)
	addis 6,0,ofname@ha
	addi 6,6,ofname@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,foreground@ha
	lwz 0,foreground@l(9)
	cmpwi 1,0,0
	bc 12,6,.L400
	lwz 9,_impure_ptr@l(29)
	lwz 3,4(9)
	bl fileno
	bl isatty
	cmpwi 1,3,0
	bc 12,6,.L400
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	addis 4,0,.LC140@ha
	addi 4,4,.LC140@l
	crxor 6,6,6
	bl fprintf
	lwz 9,_impure_ptr@l(29)
	lwz 3,12(9)
	bl fflush
	lwz 9,_impure_ptr@l(29)
	addi 3,1,72
	li 4,79
	lwz 5,4(9)
	bl fgets
.L400:
	lbz 11,72(1)
	addis 9,0,_ctype_+1@ha
	addi 9,9,_ctype_+1@l
	lbzx 0,11,9
	andi. 9,0,1
	bc 12,2,.L402
	cmpwi 1,11,89
	bc 4,6,.L403
	b .L399
.L402:
	lbz 0,72(1)
	cmpwi 1,0,121
	bc 12,6,.L399
.L403:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC141@ha
	addi 4,4,.LC141@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L410
	li 0,2
	b .L412
.L399:
	addis 29,0,ofname@ha
	addi 3,29,ofname@l
	li 4,511
	bl chmod
	addi 3,29,ofname@l
	bl unlink
	cmpwi 1,3,0
	bc 4,6,.L405
.L411:
	li 3,0
	b .L406
.L405:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl perror
.L409:
	addis 9,0,exit_code@ha
	li 0,1
.L412:
	stw 0,exit_code@l(9)
.L410:
	li 3,1
.L406:
	lwz 0,172(1)
	mtlr 0
	lwz 29,156(1)
	lwz 30,160(1)
	lwz 31,164(1)
	addi 1,1,168
	blr
.Lfe18:
	.size	 check_ofname,.Lfe18-check_ofname
	.align 2
	.type	 reset_times,@function
reset_times:
	stwu 1,-24(1)
	mflr 0
	stw 31,20(1)
	stw 0,28(1)
	mr 31,4
# Note:	the following jump instruction is to an external C function.
#
	bl cfunc
#
	lwz 0,20(31)
	stw 0,8(1)
	lwz 0,28(31)
	stw 0,12(1)
	addi 4,1,8
	crxor 6,6,6
	bl utime
	cmpwi 1,3,0
	bc 12,6,.L414
	lwz 0,4(31)
	rlwinm 0,0,0,16,19
	cmpwi 1,0,16384
	bc 12,6,.L414
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L415
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
.L415:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L416
	li 0,2
	stw 0,exit_code@l(9)
.L416:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L414
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl perror
.L414:
	lwz 0,28(1)
	mtlr 0
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe19:
	.size	 reset_times,.Lfe19-reset_times
	.section	".rodata"
	.align 2
.LC142:
	.string	"%s: time stamp restored\n"
	.section ".text"
	.align 2
	.type	 copy_stat,@function
copy_stat:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 31,3
	addis 9,0,decompress@ha
	lwz 0,decompress@l(9)
	cmpwi 1,0,0
	bc 12,6,.L419
	addis 9,0,time_stamp@ha
	lwz 9,time_stamp@l(9)
	cmpwi 1,9,0
	bc 12,6,.L419
	lwz 0,28(31)
	cmpw 1,0,9
	bc 12,6,.L419
	stw 9,28(31)
	addis 9,0,verbose@ha
	lwz 0,verbose@l(9)
	cmpwi 1,0,1
	bc 4,5,.L419
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC142@ha
	addi 4,4,.LC142@l
	addis 5,0,ofname@ha
	addi 5,5,ofname@l
	crxor 6,6,6
	bl fprintf
.L419:
	addis 29,0,ofname@ha
	addi 3,29,ofname@l
	mr 4,31
	bl reset_times
	lwz 4,4(31)
	addi 3,29,ofname@l
	rlwinm 4,4,0,20,31
	bl chmod
	cmpwi 1,3,0
	bc 12,6,.L421
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L422
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
.L422:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L423
	li 0,2
	stw 0,exit_code@l(9)
.L423:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L421
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl perror
.L421:
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	lhz 4,10(31)
	lhz 5,12(31)
	bl chown
	addis 9,0,remove_ofname@ha
	li 0,0
	stw 0,remove_ofname@l(9)
	addis 29,0,ifname@ha
	addi 3,29,ifname@l
	li 4,511
	bl chmod
	addi 3,29,ifname@l
	bl unlink
	cmpwi 1,3,0
	bc 12,6,.L425
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L426
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC86@ha
	addi 4,4,.LC86@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
.L426:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L427
	li 0,2
	stw 0,exit_code@l(9)
.L427:
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L425
	addis 3,0,ifname@ha
	addi 3,3,ifname@l
	bl perror
.L425:
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe20:
	.size	 copy_stat,.Lfe20-copy_stat
	.section ".sdata","aw"
	.align 2
	.type	 in_exit.47,@object
	.size	 in_exit.47,4
in_exit.47:
	.long 0
	.section ".text"
	.align 2
	.type	 do_exit,@function
do_exit:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	mr 30,3
	addis 9,0,in_exit.47@ha
	lwz 0,in_exit.47@l(9)
	cmpwi 1,0,0
	bc 12,6,.L430
	bl exit
.L430:
	addis 9,0,in_exit.47@ha
	li 0,1
	stw 0,in_exit.47@l(9)
	addis 31,0,env@ha
	lwz 3,env@l(31)
	cmpwi 1,3,0
	bc 12,6,.L431
	bl free
	li 0,0
	stw 0,env@l(31)
.L431:
	addis 31,0,args@ha
	lwz 3,args@l(31)
	cmpwi 1,3,0
	bc 12,6,.L432
	bl free
	li 0,0
	stw 0,args@l(31)
.L432:
	mr 3,30
	bl exit
.Lfe21:
	.size	 do_exit,.Lfe21-do_exit
	.align 2
	.globl abort_gzip
	.type	 abort_gzip,@function
abort_gzip:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,remove_ofname@ha
	lwz 0,remove_ofname@l(9)
	cmpwi 1,0,0
	bc 12,6,.L434
	addis 9,0,ofd@ha
	lwz 3,ofd@l(9)
	bl close
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl unlink
.L434:
	li 3,1
	bl do_exit
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe22:
	.size	 abort_gzip,.Lfe22-abort_gzip
	.comm	inbuf,32832,1
	.comm	outbuf,18432,1
	.comm	d_buf,65536,2
	.comm	window,65536,1
	.comm	prev,131072,2
	.comm	insize,4,4
	.comm	inptr,4,4
	.comm	outcnt,4,4
	.comm	bytes_in,4,4
	.comm	bytes_out,4,4
	.comm	ifd,4,4
	.comm	ofd,4,4
	.comm	ifname,1024,1
	.comm	ofname,1024,1
	.comm	progname,4,4
	.comm	time_stamp,4,4
	.comm	ifile_size,4,4
	.comm	save_orig_name,4,4
	.comm	foreground,4,4
	.comm	last_member,4,4
	.comm	part_nb,4,4
	.comm	env,4,4
	.comm	z_suffix,31,1
	.comm	z_len,4,4
	.comm	istat,60,4
	.ident	"GCC: (GNU) 2.7-97r2"
