	.file	"lzw.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.section ".sdata","aw"
	.align 2
	.type	 msg_done,@object
	.size	 msg_done,4
msg_done:
	.long 0
	.section	".rodata"
	.align 2
.LC0:
	.string	"output in compress .Z format not supported\n"
	.section ".text"
	.align 2
	.globl lzw
	.type	 lzw,@function
lzw:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 30,3
	mr 31,4
	addis 9,0,msg_done@ha
	lwz 0,msg_done@l(9)
	cmpwi 1,0,0
	bc 4,6,.L3
	addis 9,0,msg_done@ha
	li 29,1
	stw 29,msg_done@l(9)
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC0@ha
	addi 4,4,.LC0@l
	crxor 6,6,6
	bl fprintf
	cmpw 1,30,31
	bc 12,6,.L3
	addis 9,0,exit_code@ha
	stw 29,exit_code@l(9)
.L3:
	li 3,1
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe1:
	.size	 lzw,.Lfe1-lzw
	.ident	"GCC: (GNU) 2.7-97r2"
