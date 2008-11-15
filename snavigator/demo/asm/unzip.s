	.file	"unzip.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.globl pkzip
	.section ".sdata","aw"
	.align 2
	.type	 pkzip,@object
	.size	 pkzip,4
pkzip:
	.long 0
	.globl ext_header
	.align 2
	.type	 ext_header,@object
	.size	 ext_header,4
ext_header:
	.long 0
	.section	".rodata"
	.align 2
.LC0:
	.string	"\n%s: %s: not a valid zip file\n"
	.align 2
.LC1:
	.string	"\n%s: %s: first entry not deflated or stored -- use unzip\n"
	.align 2
.LC2:
	.string	"\n%s: %s: encrypted file -- use unzip\n"
	.section ".text"
	.align 2
	.globl check_zipfile
	.type	 check_zipfile,@function
check_zipfile:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 10,0,inptr@ha
	addis 9,0,inbuf@ha
	addi 6,9,inbuf@l
	lwz 7,inptr@l(10)
	add 8,7,6
	addis 9,0,ifd@ha
	stw 3,ifd@l(9)
	lbz 11,26(8)
	lbz 0,27(8)
	slwi 0,0,8
	or 11,11,0
	lbz 9,28(8)
	lbz 0,29(8)
	slwi 0,0,8
	or 9,9,0
	addi 9,9,30
	add 11,11,9
	add 11,11,7
	stw 11,inptr@l(10)
	addis 9,0,insize@ha
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 12,5,.L3
	lbzx 11,7,6
	lbz 0,1(8)
	slwi 0,0,8
	or 11,11,0
	lbz 9,2(8)
	lbz 0,3(8)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 11,11,9
	lis 0,0x403
	ori 0,0,19280
	cmpw 1,11,0
	bc 12,6,.L2
.L3:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC0@ha
	addi 4,4,.LC0@l
	b .L7
.L2:
	addis 9,0,method@ha
	lbz 0,8(8)
	stw 0,method@l(9)
	addic 9,0,-1
	subfe 11,9,0
	xori 9,0,8
	neg 9,9
	srwi 9,9,31
	and. 0,11,9
	bc 12,2,.L4
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC1@ha
	addi 4,4,.LC1@l
	b .L7
.L4:
	addis 9,0,decrypt@ha
	lbz 0,6(8)
	rlwinm 0,0,0,31,31
	stw 0,decrypt@l(9)
	cmpwi 1,0,0
	bc 4,6,.L5
	addis 9,0,ext_header@ha
	lbz 0,6(8)
	rlwinm 0,0,29,31,31
	stw 0,ext_header@l(9)
	addis 9,0,pkzip@ha
	li 0,1
	stw 0,pkzip@l(9)
	li 3,0
	b .L6
.L5:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC2@ha
	addi 4,4,.LC2@l
.L7:
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	mr 3,0
.L6:
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe1:
	.size	 check_zipfile,.Lfe1-check_zipfile
	.section	".rodata"
	.align 2
.LC3:
	.string	"out of memory"
	.align 2
.LC4:
	.string	"invalid compressed data--format violated"
	.align 2
.LC5:
	.string	"len %ld, siz %ld\n"
	.align 2
.LC6:
	.string	"invalid compressed data--length mismatch"
	.align 2
.LC7:
	.string	"internal error, invalid method"
	.align 2
.LC8:
	.string	"invalid compressed data--crc error"
	.align 2
.LC9:
	.string	"invalid compressed data--length error"
	.align 2
.LC10:
	.string	"%s: %s has more than one entry--rest ignored\n"
	.align 2
.LC11:
	.string	"%s: %s has more than one entry -- unchanged\n"
	.section ".text"
	.align 2
	.globl unzip
	.type	 unzip,@function
unzip:
	stwu 1,-64(1)
	mflr 0
	stw 23,28(1)
	stw 24,32(1)
	stw 25,36(1)
	stw 26,40(1)
	stw 27,44(1)
	stw 28,48(1)
	stw 29,52(1)
	stw 30,56(1)
	stw 31,60(1)
	stw 0,68(1)
	li 29,0
	mr 30,29
	addis 9,0,ifd@ha
	stw 3,ifd@l(9)
	addis 9,0,ofd@ha
	stw 4,ofd@l(9)
	mr 3,29
	mr 4,29
	bl updcrc
	addis 9,0,pkzip@ha
	lwz 0,pkzip@l(9)
	cmpw 1,0,29
	bc 12,6,.L9
	addis 9,0,ext_header@ha
	lwz 0,ext_header@l(9)
	cmpw 1,0,29
	bc 4,6,.L9
	addis 9,0,inbuf+14@ha
	addi 11,9,inbuf+14@l
	lbz 29,inbuf+14@l(9)
	lbz 0,1(11)
	slwi 0,0,8
	or 29,29,0
	lbz 9,2(11)
	lbz 0,3(11)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 29,29,9
	lbz 30,8(11)
	lbz 0,9(11)
	slwi 0,0,8
	or 30,30,0
	lbz 9,10(11)
	lbz 0,11(11)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 30,30,9
.L9:
	addis 9,0,method@ha
	lwz 0,method@l(9)
	cmpwi 1,0,8
	bc 4,6,.L10
	bl inflate
	cmpwi 1,3,3
	bc 4,6,.L11
	addis 3,0,.LC3@ha
	addi 3,3,.LC3@l
	b .L52
.L11:
	cmpwi 1,3,0
	bc 12,6,.L14
	addis 3,0,.LC4@ha
	addi 3,3,.LC4@l
	b .L52
.L10:
	addis 9,0,pkzip@ha
	lwz 0,pkzip@l(9)
	cmpwi 1,0,0
	bc 12,6,.L15
	addis 9,0,method@ha
	lwz 0,method@l(9)
	cmpwi 1,0,0
	bc 4,6,.L15
	addis 9,0,inbuf+22@ha
	addi 11,9,inbuf+22@l
	lbz 31,inbuf+22@l(9)
	lbz 0,1(11)
	slwi 0,0,8
	or 31,31,0
	lbz 9,2(11)
	lbz 0,3(11)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 31,31,9
	lbz 10,-4(11)
	lbz 0,-3(11)
	slwi 0,0,8
	or 10,10,0
	lbz 9,-2(11)
	lbz 0,-1(11)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 10,10,9
	mr 11,31
	addis 9,0,decrypt@ha
	lwz 0,decrypt@l(9)
	cmpwi 1,0,0
	bc 12,6,.L17
	addi 0,10,-12
	cmpw 1,31,0
	bc 4,6,.L18
	b .L16
.L17:
	cmpw 1,11,10
	bc 12,6,.L16
.L18:
	addis 9,0,_impure_ptr@ha
	lwz 10,_impure_ptr@l(9)
	addis 9,0,inbuf+18@ha
	addi 11,9,inbuf+18@l
	lbz 9,inbuf+18@l(9)
	lbz 0,1(11)
	slwi 0,0,8
	or 9,9,0
	lbz 6,2(11)
	lbz 0,3(11)
	slwi 0,0,8
	or 6,6,0
	slwi 6,6,16
	lwz 3,12(10)
	addis 4,0,.LC5@ha
	addi 4,4,.LC5@l
	mr 5,31
	or 6,9,6
	crxor 6,6,6
	bl fprintf
	addis 3,0,.LC6@ha
	addi 3,3,.LC6@l
	bl error
.L16:
	mr 0,31
	addi 31,31,-1
	cmpwi 1,0,0
	bc 12,6,.L20
	addis 27,0,inptr@ha
	addis 23,0,insize@ha
	addis 9,0,inbuf@ha
	addi 24,9,inbuf@l
	addis 9,0,window@ha
	addi 25,9,window@l
	addis 28,0,outcnt@ha
	li 26,0
	ori 26,26,32768
.L21:
	lwz 9,inptr@l(27)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L22
	lbzx 3,24,9
	addi 0,9,1
	stw 0,inptr@l(27)
	b .L23
.L22:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
.L23:
	lwz 0,outcnt@l(28)
	stbx 3,25,0
	addic 0,0,1
	stw 0,outcnt@l(28)
	cmpw 1,0,26
	bc 4,6,.L19
	bl flush_window
.L19:
	mr 0,31
	addi 31,31,-1
	cmpwi 1,0,0
	bc 4,6,.L21
.L20:
	bl flush_window
	b .L14
.L15:
	addis 3,0,.LC7@ha
	addi 3,3,.LC7@l
.L52:
	bl error
.L14:
	addis 9,0,pkzip@ha
	lwz 0,pkzip@l(9)
	cmpwi 1,0,0
	bc 4,6,.L27
	li 31,0
	addis 30,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
	addi 29,1,8
.L31:
	lwz 9,inptr@l(30)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L32
	lbzx 3,28,9
	addi 0,9,1
	stw 0,inptr@l(30)
	b .L33
.L32:
	li 3,0
	bl fill_inbuf
.L33:
	stbx 3,29,31
	addi 31,31,1
	cmpwi 1,31,7
	bc 4,5,.L31
	lbz 29,8(1)
	lbz 0,9(1)
	slwi 0,0,8
	or 29,29,0
	lbz 9,10(1)
	lbz 0,11(1)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 29,29,9
	lbz 30,12(1)
	lbz 0,13(1)
	slwi 0,0,8
	or 30,30,0
	lbz 9,14(1)
	lbz 0,15(1)
	b .L53
.L27:
	addis 9,0,ext_header@ha
	lwz 0,ext_header@l(9)
	cmpwi 1,0,0
	bc 12,6,.L35
	li 31,0
	addis 30,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
	addi 29,1,8
.L40:
	lwz 9,inptr@l(30)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L41
	lbzx 3,28,9
	addi 0,9,1
	stw 0,inptr@l(30)
	b .L42
.L41:
	li 3,0
	bl fill_inbuf
.L42:
	stbx 3,29,31
	addi 31,31,1
	cmpwi 1,31,15
	bc 4,5,.L40
	lbz 29,12(1)
	lbz 0,13(1)
	slwi 0,0,8
	or 29,29,0
	lbz 9,14(1)
	lbz 0,15(1)
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 29,29,9
	lbz 30,20(1)
	lbz 0,21(1)
	slwi 0,0,8
	or 30,30,0
	lbz 9,22(1)
	lbz 0,23(1)
.L53:
	slwi 0,0,8
	or 9,9,0
	slwi 9,9,16
	or 30,30,9
.L35:
	addis 3,0,outbuf@ha
	addi 3,3,outbuf@l
	li 4,0
	bl updcrc
	cmpw 1,29,3
	bc 12,6,.L44
	addis 3,0,.LC8@ha
	addi 3,3,.LC8@l
	bl error
.L44:
	addis 9,0,bytes_out@ha
	lwz 0,bytes_out@l(9)
	cmpw 1,30,0
	bc 12,6,.L45
	addis 3,0,.LC9@ha
	addi 3,3,.LC9@l
	bl error
.L45:
	addis 9,0,pkzip@ha
	lwz 0,pkzip@l(9)
	cmpwi 1,0,0
	bc 12,6,.L46
	addis 9,0,inptr@ha
	lwz 8,inptr@l(9)
	addi 11,8,4
	addis 9,0,insize@ha
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L46
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 10,8,9
	addi 11,9,1
	lbzx 0,8,11
	slwi 0,0,8
	or 10,10,0
	addi 11,9,2
	lbzx 11,8,11
	addi 9,9,3
	lbzx 0,8,9
	slwi 0,0,8
	or 11,11,0
	slwi 11,11,16
	or 10,10,11
	lis 0,0x403
	ori 0,0,19280
	cmpw 1,10,0
	bc 4,6,.L46
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L47
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L48
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC10@ha
	addi 4,4,.LC10@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
.L48:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L46
	li 0,2
	stw 0,exit_code@l(9)
	b .L46
.L47:
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC11@ha
	addi 4,4,.LC11@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	li 0,1
	stw 0,exit_code@l(9)
	addis 11,0,ext_header@ha
	addis 9,0,pkzip@ha
	li 0,0
	stw 0,pkzip@l(9)
	stw 0,ext_header@l(11)
	li 3,1
	b .L51
.L46:
	addis 11,0,ext_header@ha
	addis 9,0,pkzip@ha
	li 0,0
	stw 0,pkzip@l(9)
	stw 0,ext_header@l(11)
	mr 3,0
.L51:
	lwz 0,68(1)
	mtlr 0
	lwz 23,28(1)
	lwz 24,32(1)
	lwz 25,36(1)
	lwz 26,40(1)
	lwz 27,44(1)
	lwz 28,48(1)
	lwz 29,52(1)
	lwz 30,56(1)
	lwz 31,60(1)
	addi 1,1,64
	blr
.Lfe2:
	.size	 unzip,.Lfe2-unzip
	.comm	decrypt,4,4
	.comm	key,4,4
	.ident	"GCC: (GNU) 2.7-97r2"
