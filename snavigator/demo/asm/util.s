	.file	"util.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.section ".text"
	.align 2
	.globl copy
	.type	 copy,@function
copy:
	stwu 1,-32(1)
	mflr 0
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 27,3
	mr 28,4
	bl __errno
	li 0,0
	stw 0,0(3)
	addis 9,0,insize@ha
	lwz 0,insize@l(9)
	cmpwi 1,0,0
	bc 12,6,.L3
	mr 31,9
	addis 29,0,inbuf@ha
	addis 30,0,bytes_out@ha
.L6:
	lwz 5,insize@l(31)
	cmpwi 1,5,-1
	bc 12,6,.L8
	mr 3,28
	addi 4,29,inbuf@l
	bl write_buf
	lwz 0,bytes_out@l(30)
	lwz 9,insize@l(31)
	add 0,0,9
	stw 0,bytes_out@l(30)
	mr 3,27
	addi 4,29,inbuf@l
	li 5,0
	ori 5,5,32768
	bl read
	stw 3,insize@l(31)
	cmpwi 1,3,0
	bc 4,6,.L6
.L3:
	addis 9,0,insize@ha
	lwz 0,insize@l(9)
	cmpwi 1,0,-1
	bc 4,6,.L7
.L8:
	bl __errno
	lwz 0,0(3)
	cmpwi 1,0,0
	bc 12,6,.L7
	bl read_error
.L7:
	addis 9,0,bytes_in@ha
	addis 11,0,bytes_out@ha
	lwz 0,bytes_out@l(11)
	stw 0,bytes_in@l(9)
	li 3,0
	lwz 0,36(1)
	mtlr 0
	lwz 27,12(1)
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe1:
	.size	 copy,.Lfe1-copy
	.section ".sdata","aw"
	.align 2
	.type	 crc.4,@object
	.size	 crc.4,4
crc.4:
	.long -1
	.section ".text"
	.align 2
	.globl updcrc
	.type	 updcrc,@function
updcrc:
	mr. 11,3
	bc 4,2,.L10
	li 3,-1
	b .L11
.L10:
	addis 9,0,crc.4@ha
	lwz 3,crc.4@l(9)
	cmpwi 1,4,0
	bc 12,6,.L11
	addis 9,0,crc_32_tab@ha
	addi 10,9,crc_32_tab@l
.L13:
	lbz 0,0(11)
	xor 0,3,0
	rlwinm 0,0,2,22,29
	srwi 9,3,8
	lwzx 0,10,0
	xor 3,9,0
	addi 11,11,1
	addic. 4,4,-1
	bc 4,2,.L13
.L11:
	addis 9,0,crc.4@ha
	stw 3,crc.4@l(9)
	nor 3,3,3
	blr
.Lfe2:
	.size	 updcrc,.Lfe2-updcrc
	.align 2
	.globl clear_bufs
	.type	 clear_bufs,@function
clear_bufs:
	addis 9,0,outcnt@ha
	li 0,0
	stw 0,outcnt@l(9)
	addis 11,0,insize@ha
	addis 9,0,inptr@ha
	stw 0,inptr@l(9)
	stw 0,insize@l(11)
	addis 11,0,bytes_in@ha
	addis 9,0,bytes_out@ha
	stw 0,bytes_out@l(9)
	stw 0,bytes_in@l(11)
	blr
.Lfe3:
	.size	 clear_bufs,.Lfe3-clear_bufs
	.align 2
	.globl fill_inbuf
	.type	 fill_inbuf,@function
fill_inbuf:
	stwu 1,-32(1)
	mflr 0
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 27,3
	addis 9,0,insize@ha
	li 29,0
	stw 29,insize@l(9)
	bl __errno
	stw 29,0(3)
	addis 28,0,ifd@ha
	addis 29,0,insize@ha
	addis 9,0,inbuf@ha
	addi 30,9,inbuf@l
	li 31,0
	ori 31,31,32768
.L19:
	lwz 5,insize@l(29)
	lwz 3,ifd@l(28)
	add 4,5,30
	subf 5,5,31
	bl read
	addi 0,3,1
	cmplwi 1,0,1
	bc 4,5,.L20
	lwz 0,insize@l(29)
	add 0,3,0
	stw 0,insize@l(29)
	cmplwi 1,0,32767
	bc 4,5,.L19
.L20:
	addis 9,0,insize@ha
	lwz 0,insize@l(9)
	cmpwi 1,0,0
	bc 4,6,.L24
	cmpwi 1,27,0
	bc 12,6,.L25
	li 3,-1
	b .L26
.L25:
	bl read_error
.L24:
	addis 11,0,bytes_in@ha
	addis 9,0,insize@ha
	lwz 0,bytes_in@l(11)
	lwz 9,insize@l(9)
	add 0,0,9
	stw 0,bytes_in@l(11)
	addis 9,0,inptr@ha
	li 0,1
	stw 0,inptr@l(9)
	addis 9,0,inbuf@ha
	lbz 3,inbuf@l(9)
.L26:
	lwz 0,36(1)
	mtlr 0
	lwz 27,12(1)
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe4:
	.size	 fill_inbuf,.Lfe4-fill_inbuf
	.align 2
	.globl flush_outbuf
	.type	 flush_outbuf,@function
flush_outbuf:
	stwu 1,-16(1)
	mflr 0
	stw 31,12(1)
	stw 0,20(1)
	addis 31,0,outcnt@ha
	lwz 5,outcnt@l(31)
	cmpwi 1,5,0
	bc 12,6,.L27
	addis 9,0,ofd@ha
	lwz 3,ofd@l(9)
	addis 4,0,outbuf@ha
	addi 4,4,outbuf@l
	bl write_buf
	addis 11,0,bytes_out@ha
	lwz 0,bytes_out@l(11)
	lwz 9,outcnt@l(31)
	add 0,0,9
	stw 0,bytes_out@l(11)
	li 0,0
	stw 0,outcnt@l(31)
.L27:
	lwz 0,20(1)
	mtlr 0
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe5:
	.size	 flush_outbuf,.Lfe5-flush_outbuf
	.align 2
	.globl flush_window
	.type	 flush_window,@function
flush_window:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	addis 30,0,outcnt@ha
	lwz 4,outcnt@l(30)
	cmpwi 1,4,0
	bc 12,6,.L29
	addis 31,0,window@ha
	addi 3,31,window@l
	bl updcrc
	addis 9,0,test@ha
	lwz 0,test@l(9)
	cmpwi 1,0,0
	bc 4,6,.L31
	addis 9,0,ofd@ha
	lwz 3,ofd@l(9)
	addi 4,31,window@l
	lwz 5,outcnt@l(30)
	bl write_buf
.L31:
	addis 11,0,bytes_out@ha
	addis 10,0,outcnt@ha
	lwz 0,bytes_out@l(11)
	lwz 9,outcnt@l(10)
	add 0,0,9
	stw 0,bytes_out@l(11)
	li 0,0
	stw 0,outcnt@l(10)
.L29:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe6:
	.size	 flush_window,.Lfe6-flush_window
	.align 2
	.globl write_buf
	.type	 write_buf,@function
write_buf:
	stwu 1,-24(1)
	mflr 0
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 28,3
	mr 29,4
	mr 30,5
.L33:
	mr 3,28
	mr 4,29
	mr 5,30
	bl write
	mr 31,3
	cmpw 1,31,30
	bc 12,6,.L34
	cmpwi 1,31,-1
	bc 4,6,.L36
	bl write_error
.L36:
	subf 30,31,30
	add 29,29,31
	b .L33
.L34:
	lwz 0,28(1)
	mtlr 0
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe7:
	.size	 write_buf,.Lfe7-write_buf
	.align 2
	.globl strlwr
	.type	 strlwr,@function
strlwr:
	mr 10,3
	lbz 0,0(3)
	cmpwi 1,0,0
	bclr 12,6
	addis 9,0,_ctype_+1@ha
	addi 8,9,_ctype_+1@l
.L41:
	lbz 11,0(10)
	rlwinm 0,11,0,0xff
	lbzx 0,8,0
	andi. 9,0,1
	bc 12,2,.L42
	addi 0,11,32
	b .L43
.L42:
	lbz 0,0(10)
.L43:
	stb 0,0(10)
	lbzu 0,1(10)
	cmpwi 1,0,0
	bc 4,6,.L41
	blr
.Lfe8:
	.size	 strlwr,.Lfe8-strlwr
	.align 2
	.globl basename
	.type	 basename,@function
basename:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 29,3
	li 4,47
	bl strrchr
	addic 0,3,-1
	subfe 0,0,0
	addi 3,3,1
	and 29,29,0
	andc 0,3,0
	or 3,29,0
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe9:
	.size	 basename,.Lfe9-basename
	.align 2
	.globl make_simple_name
	.type	 make_simple_name,@function
make_simple_name:
	stwu 1,-16(1)
	mflr 0
	stw 31,12(1)
	stw 0,20(1)
	mr 31,3
	li 4,46
	bl strrchr
	mr. 3,3
	bc 12,2,.L48
	xor 0,3,31
	srawi 9,0,31
	xor 0,9,0
	subf 0,0,9
	srawi 0,0,31
	addi 9,3,1
	and 11,3,0
	andc 0,9,0
	or 3,11,0
	li 9,95
.L51:
	lbzu 0,-1(3)
	cmpwi 1,0,46
	bc 4,6,.L53
	stb 9,0(3)
.L53:
	cmpw 1,3,31
	bc 4,6,.L51
.L48:
	lwz 0,20(1)
	mtlr 0
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe10:
	.size	 make_simple_name,.Lfe10-make_simple_name
	.section	".rodata"
	.align 2
.LC0:
	.string	" \t"
	.align 2
.LC1:
	.string	"out of memory"
	.align 2
.LC2:
	.string	"argc<=0"
	.section ".text"
	.align 2
	.globl add_envopt
	.type	 add_envopt,@function
add_envopt:
	stwu 1,-40(1)
	mflr 0
	stw 24,8(1)
	stw 25,12(1)
	stw 26,16(1)
	stw 27,20(1)
	stw 28,24(1)
	stw 29,28(1)
	stw 30,32(1)
	stw 31,36(1)
	stw 0,44(1)
	mr 29,3
	mr 24,4
	lwz 27,0(29)
	li 28,0
	mr 3,5
	bl getenv
	mr. 26,3
	mr 3,28
	bc 12,2,.L81
	mr 3,26
	bl strlen
	addi 3,3,1
	bl xmalloc
	mr 4,26
	bl strcpy
	mr 26,3
	mr 31,26
	lbz 0,0(26)
	cmpw 1,0,28
	bc 12,6,.L59
	addis 30,0,.LC0@ha
	mr 25,28
.L61:
	mr 3,31
	addi 4,30,.LC0@l
	bl strspn
	lbzux 0,31,3
	cmpwi 1,0,0
	bc 12,6,.L59
	mr 3,31
	addi 4,30,.LC0@l
	bl strcspn
	lbzux 0,31,3
	cmpwi 1,0,0
	bc 12,6,.L60
	stb 25,0(31)
	addi 31,31,1
.L60:
	addi 28,28,1
	lbz 0,0(31)
	cmpwi 1,0,0
	bc 4,6,.L61
.L59:
	cmpwi 1,28,0
	bc 4,6,.L65
	mr 3,26
	bl free
	li 3,0
	b .L81
.L65:
	lwz 3,0(29)
	add 3,28,3
	stw 3,0(29)
	addi 3,3,1
	li 4,4
	bl calloc
	mr. 30,3
	bc 4,2,.L66
	addis 3,0,.LC1@ha
	addi 3,3,.LC1@l
	bl error
.L66:
	lwz 29,0(24)
	stw 30,0(24)
	mr 0,27
	addi 27,27,-1
	cmpwi 1,0,0
	bc 4,4,.L67
	addis 3,0,.LC2@ha
	addi 3,3,.LC2@l
	bl error
.L67:
	lwz 0,0(29)
	stw 0,0(30)
	addi 29,29,4
	addi 30,30,4
	mr 31,26
	cmpwi 1,28,0
	bc 4,5,.L83
	addis 25,0,.LC0@ha
.L71:
	mr 3,31
	addi 4,25,.LC0@l
	bl strspn
	add 31,31,3
	stw 31,0(30)
	addi 30,30,4
.L72:
	lbz 0,0(31)
	addi 31,31,1
	cmpwi 1,0,0
	bc 4,6,.L72
	addic. 28,28,-1
	bc 12,1,.L71
	b .L83
.L79:
	lwz 0,0(29)
	stw 0,0(30)
	addi 29,29,4
	addi 30,30,4
.L83:
	cmpwi 1,27,0
	addi 27,27,-1
	bc 4,6,.L79
	li 0,0
	stw 0,0(30)
	mr 3,26
.L81:
	lwz 0,44(1)
	mtlr 0
	lwz 24,8(1)
	lwz 25,12(1)
	lwz 26,16(1)
	lwz 27,20(1)
	lwz 28,24(1)
	lwz 29,28(1)
	lwz 30,32(1)
	lwz 31,36(1)
	addi 1,1,40
	blr
.Lfe11:
	.size	 add_envopt,.Lfe11-add_envopt
	.section	".rodata"
	.align 2
.LC3:
	.string	"\n%s: %s: %s\n"
	.section ".text"
	.align 2
	.globl error
	.type	 error,@function
error:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	mr 7,3
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC3@ha
	addi 4,4,.LC3@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	bl abort_gzip
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe12:
	.size	 error,.Lfe12-error
	.section	".rodata"
	.align 2
.LC4:
	.string	"%s: %s: warning: %s%s\n"
	.section ".text"
	.align 2
	.globl warn
	.type	 warn,@function
warn:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	mr 7,3
	mr 8,4
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L86
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC4@ha
	addi 4,4,.LC4@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
.L86:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L87
	li 0,2
	stw 0,exit_code@l(9)
.L87:
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe13:
	.size	 warn,.Lfe13-warn
	.section	".rodata"
	.align 2
.LC5:
	.string	"\n%s: "
	.align 2
.LC6:
	.string	"%s: unexpected end of file\n"
	.section ".text"
	.align 2
	.globl read_error
	.type	 read_error,@function
read_error:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC5@ha
	addi 4,4,.LC5@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
	bl __errno
	lwz 0,0(3)
	cmpwi 1,0,0
	bc 12,6,.L89
	addis 3,0,ifname@ha
	addi 3,3,ifname@l
	bl perror
	b .L90
.L89:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC6@ha
	addi 4,4,.LC6@l
	addis 5,0,ifname@ha
	addi 5,5,ifname@l
	crxor 6,6,6
	bl fprintf
.L90:
	bl abort_gzip
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe14:
	.size	 read_error,.Lfe14-read_error
	.align 2
	.globl write_error
	.type	 write_error,@function
write_error:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC5@ha
	addi 4,4,.LC5@l
	lwz 5,progname@l(11)
	crxor 6,6,6
	bl fprintf
	addis 3,0,ofname@ha
	addi 3,3,ofname@l
	bl perror
	bl abort_gzip
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe15:
	.size	 write_error,.Lfe15-write_error
	.section	".rodata"
	.align 2
.LC7:
	.string	"%2ld.%1ld%%"
	.section ".text"
	.align 2
	.globl display_ratio
	.type	 display_ratio,@function
display_ratio:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	mr 31,5
	mr. 4,4
	bc 4,2,.L93
	li 30,0
	b .L94
.L93:
	lis 0,0x20
	ori 0,0,50330
	cmpw 1,4,0
	bc 12,5,.L95
	slwi 0,3,5
	subf 0,3,0
	slwi 0,0,2
	add 0,0,3
	slwi 0,0,3
	divw 30,0,4
	b .L94
.L95:
	lis 0,0x1062
	ori 0,0,19923
	mulhw 0,4,0
	srawi 0,0,6
	srawi 9,4,31
	subf 0,9,0
	divw 30,3,0
.L94:
	cmpwi 1,30,0
	bc 4,4,.L97
	lwz 0,8(31)
	addic 9,0,-1
	stw 9,8(31)
	cmpwi 1,9,0
	bc 4,4,.L98
	lwz 0,24(31)
	cmpw 1,9,0
	bc 12,4,.L100
	lwz 9,0(31)
	li 0,45
	stb 0,0(9)
	lwz 9,0(31)
	lbz 0,0(9)
	cmpwi 1,0,10
	bc 12,6,.L102
	addi 0,9,1
	b .L111
.L102:
	li 3,10
	b .L112
.L100:
	li 3,45
.L112:
	mr 4,31
	bl __swbuf
	b .L99
.L98:
	lwz 9,0(31)
	li 0,45
	stb 0,0(9)
	lwz 0,0(31)
	addic 0,0,1
.L111:
	stw 0,0(31)
.L99:
	neg 30,30
	b .L104
.L97:
	lwz 0,8(31)
	addic 9,0,-1
	stw 9,8(31)
	cmpwi 1,9,0
	bc 4,4,.L105
	lwz 0,24(31)
	cmpw 1,9,0
	bc 12,4,.L107
	lwz 9,0(31)
	li 0,32
	stb 0,0(9)
	lwz 9,0(31)
	lbz 0,0(9)
	cmpwi 1,0,10
	bc 12,6,.L109
	addi 0,9,1
	b .L113
.L109:
	li 3,10
	b .L114
.L107:
	li 3,32
.L114:
	mr 4,31
	bl __swbuf
	b .L104
.L105:
	lwz 9,0(31)
	li 0,32
	stb 0,0(9)
	lwz 0,0(31)
	addic 0,0,1
.L113:
	stw 0,0(31)
.L104:
	lis 5,0x6666
	ori 5,5,26215
	mulhw 5,30,5
	srawi 5,5,2
	srawi 0,30,31
	subf 5,0,5
	slwi 6,5,3
	add 6,6,5
	add 6,6,5
	mr 3,31
	addis 4,0,.LC7@ha
	addi 4,4,.LC7@l
	subf 6,6,30
	crxor 6,6,6
	bl fprintf
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe16:
	.size	 display_ratio,.Lfe16-display_ratio
	.align 2
	.globl xmalloc
	.type	 xmalloc,@function
xmalloc:
	stwu 1,-16(1)
	mflr 0
	stw 31,12(1)
	stw 0,20(1)
	bl malloc
	mr. 31,3
	bc 4,2,.L116
	addis 3,0,.LC1@ha
	addi 3,3,.LC1@l
	bl error
.L116:
	mr 3,31
	lwz 0,20(1)
	mtlr 0
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe17:
	.size	 xmalloc,.Lfe17-xmalloc
	.globl crc_32_tab
	.section ".data"
	.align 2
	.type	 crc_32_tab,@object
crc_32_tab:
	.long 0
	.long 1996959894
	.long -301047508
	.long -1727442502
	.long 124634137
	.long 1886057615
	.long -379345611
	.long -1637575261
	.long 249268274
	.long 2044508324
	.long -522852066
	.long -1747789432
	.long 162941995
	.long 2125561021
	.long -407360249
	.long -1866523247
	.long 498536548
	.long 1789927666
	.long -205950648
	.long -2067906082
	.long 450548861
	.long 1843258603
	.long -187386543
	.long -2083289657
	.long 325883990
	.long 1684777152
	.long -43845254
	.long -1973040660
	.long 335633487
	.long 1661365465
	.long -99664541
	.long -1928851979
	.long 997073096
	.long 1281953886
	.long -715111964
	.long -1570279054
	.long 1006888145
	.long 1258607687
	.long -770865667
	.long -1526024853
	.long 901097722
	.long 1119000684
	.long -608450090
	.long -1396901568
	.long 853044451
	.long 1172266101
	.long -589951537
	.long -1412350631
	.long 651767980
	.long 1373503546
	.long -925412992
	.long -1076862698
	.long 565507253
	.long 1454621731
	.long -809855591
	.long -1195530993
	.long 671266974
	.long 1594198024
	.long -972236366
	.long -1324619484
	.long 795835527
	.long 1483230225
	.long -1050600021
	.long -1234817731
	.long 1994146192
	.long 31158534
	.long -1731059524
	.long -271249366
	.long 1907459465
	.long 112637215
	.long -1614814043
	.long -390540237
	.long 2013776290
	.long 251722036
	.long -1777751922
	.long -519137256
	.long 2137656763
	.long 141376813
	.long -1855689577
	.long -429695999
	.long 1802195444
	.long 476864866
	.long -2056965928
	.long -228458418
	.long 1812370925
	.long 453092731
	.long -2113342271
	.long -183516073
	.long 1706088902
	.long 314042704
	.long -1950435094
	.long -54949764
	.long 1658658271
	.long 366619977
	.long -1932296973
	.long -69972891
	.long 1303535960
	.long 984961486
	.long -1547960204
	.long -725929758
	.long 1256170817
	.long 1037604311
	.long -1529756563
	.long -740887301
	.long 1131014506
	.long 879679996
	.long -1385723834
	.long -631195440
	.long 1141124467
	.long 855842277
	.long -1442165665
	.long -586318647
	.long 1342533948
	.long 654459306
	.long -1106571248
	.long -921952122
	.long 1466479909
	.long 544179635
	.long -1184443383
	.long -832445281
	.long 1591671054
	.long 702138776
	.long -1328506846
	.long -942167884
	.long 1504918807
	.long 783551873
	.long -1212326853
	.long -1061524307
	.long -306674912
	.long -1698712650
	.long 62317068
	.long 1957810842
	.long -355121351
	.long -1647151185
	.long 81470997
	.long 1943803523
	.long -480048366
	.long -1805370492
	.long 225274430
	.long 2053790376
	.long -468791541
	.long -1828061283
	.long 167816743
	.long 2097651377
	.long -267414716
	.long -2029476910
	.long 503444072
	.long 1762050814
	.long -144550051
	.long -2140837941
	.long 426522225
	.long 1852507879
	.long -19653770
	.long -1982649376
	.long 282753626
	.long 1742555852
	.long -105259153
	.long -1900089351
	.long 397917763
	.long 1622183637
	.long -690576408
	.long -1580100738
	.long 953729732
	.long 1340076626
	.long -776247311
	.long -1497606297
	.long 1068828381
	.long 1219638859
	.long -670225446
	.long -1358292148
	.long 906185462
	.long 1090812512
	.long -547295293
	.long -1469587627
	.long 829329135
	.long 1181335161
	.long -882789492
	.long -1134132454
	.long 628085408
	.long 1382605366
	.long -871598187
	.long -1156888829
	.long 570562233
	.long 1426400815
	.long -977650754
	.long -1296233688
	.long 733239954
	.long 1555261956
	.long -1026031705
	.long -1244606671
	.long 752459403
	.long 1541320221
	.long -1687895376
	.long -328994266
	.long 1969922972
	.long 40735498
	.long -1677130071
	.long -351390145
	.long 1913087877
	.long 83908371
	.long -1782625662
	.long -491226604
	.long 2075208622
	.long 213261112
	.long -1831694693
	.long -438977011
	.long 2094854071
	.long 198958881
	.long -2032938284
	.long -237706686
	.long 1759359992
	.long 534414190
	.long -2118248755
	.long -155638181
	.long 1873836001
	.long 414664567
	.long -2012718362
	.long -15766928
	.long 1711684554
	.long 285281116
	.long -1889165569
	.long -127750551
	.long 1634467795
	.long 376229701
	.long -1609899400
	.long -686959890
	.long 1308918612
	.long 956543938
	.long -1486412191
	.long -799009033
	.long 1231636301
	.long 1047427035
	.long -1362007478
	.long -640263460
	.long 1088359270
	.long 936918000
	.long -1447252397
	.long -558129467
	.long 1202900863
	.long 817233897
	.long -1111625188
	.long -893730166
	.long 1404277552
	.long 615818150
	.long -1160759803
	.long -841546093
	.long 1423857449
	.long 601450431
	.long -1285129682
	.long -1000256840
	.long 1567103746
	.long 711928724
	.long -1274298825
	.long -1022587231
	.long 1510334235
	.long 755167117
	.size	 crc_32_tab,1024
	.ident	"GCC: (GNU) 2.7-97r2"
