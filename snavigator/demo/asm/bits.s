	.file	"bits.c"

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
	.globl bi_init
	.type	 bi_init,@function
bi_init:
	addis 9,0,zfile@ha
	stw 3,zfile@l(9)
	addis 9,0,bi_buf@ha
	li 0,0
	sth 0,bi_buf@l(9)
	addis 9,0,bi_valid@ha
	li 0,0
	stw 0,bi_valid@l(9)
	cmpwi 1,3,-1
	bclr 12,6
	addis 11,0,read_buf@ha
	addis 9,0,file_read@ha
	addi 9,9,file_read@l
	stw 9,read_buf@l(11)
	blr
.Lfe1:
	.size	 bi_init,.Lfe1-bi_init
	.align 2
	.globl send_bits
	.type	 send_bits,@function
send_bits:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	mr 31,3
	mr 30,4
	addis 9,0,bi_valid@ha
	subfic 0,30,16
	lwz 9,bi_valid@l(9)
	cmpw 1,9,0
	bc 4,5,.L4
	addis 10,0,bi_buf@ha
	addi 7,10,bi_buf@l
	slw 0,31,9
	lhz 9,bi_buf@l(10)
	or 9,9,0
	sth 9,bi_buf@l(10)
	addis 8,0,outcnt@ha
	lwz 9,outcnt@l(8)
	cmplwi 1,9,16381
	bc 12,5,.L5
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	lbz 0,1(7)
	stbx 0,11,9
	addi 9,9,1
	stw 9,outcnt@l(8)
	lhz 0,bi_buf@l(10)
	srwi 0,0,8
	stbx 0,11,9
	addi 9,9,1
	stw 9,outcnt@l(8)
	b .L6
.L5:
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	addis 9,0,bi_buf+1@ha
	lbz 9,bi_buf+1@l(9)
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	cmpwi 1,0,16384
	bc 4,6,.L7
	bl flush_outbuf
.L7:
	addis 10,0,outbuf@ha
	addi 10,10,outbuf@l
	addis 8,0,outcnt@ha
	lwz 11,outcnt@l(8)
	addis 9,0,bi_buf@ha
	lhz 0,bi_buf@l(9)
	srwi 0,0,8
	stbx 0,10,11
	addi 11,11,1
	stw 11,outcnt@l(8)
	cmpwi 1,11,16384
	bc 4,6,.L6
	bl flush_outbuf
.L6:
	addis 8,0,bi_buf@ha
	rlwinm 11,31,0,0xffff
	addis 10,0,bi_valid@ha
	lwz 0,bi_valid@l(10)
	subfic 9,0,16
	sraw 11,11,9
	sth 11,bi_buf@l(8)
	addic 0,0,-16
	add 0,0,30
	stw 0,bi_valid@l(10)
	b .L9
.L4:
	addis 10,0,bi_buf@ha
	addis 8,0,bi_valid@ha
	lwz 9,bi_valid@l(8)
	slw 11,31,9
	lhz 0,bi_buf@l(10)
	or 0,0,11
	sth 0,bi_buf@l(10)
	add 9,30,9
	stw 9,bi_valid@l(8)
.L9:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe2:
	.size	 send_bits,.Lfe2-send_bits
	.align 2
	.globl bi_reverse
	.type	 bi_reverse,@function
bi_reverse:
	mr 9,3
	li 3,0
.L11:
	rlwinm 0,9,0,31,31
	or 3,3,0
	srwi 9,9,1
	slwi 3,3,1
	addic. 4,4,-1
	bc 12,1,.L11
	srwi 3,3,1
	blr
.Lfe3:
	.size	 bi_reverse,.Lfe3-bi_reverse
	.align 2
	.globl bi_windup
	.type	 bi_windup,@function
bi_windup:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,bi_valid@ha
	lwz 0,bi_valid@l(9)
	cmpwi 1,0,8
	bc 4,5,.L16
	addis 7,0,outcnt@ha
	lwz 8,outcnt@l(7)
	cmplwi 1,8,16381
	bc 12,5,.L17
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,bi_buf@ha
	addi 9,10,bi_buf@l
	lbz 0,1(9)
	stbx 0,11,8
	addi 9,8,1
	stw 9,outcnt@l(7)
	lhz 0,bi_buf@l(10)
	srwi 0,0,8
	stbx 0,11,9
	addi 9,9,1
	stw 9,outcnt@l(7)
	b .L21
.L17:
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	addis 9,0,bi_buf+1@ha
	lbz 9,bi_buf+1@l(9)
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	cmpwi 1,0,16384
	bc 4,6,.L19
	bl flush_outbuf
.L19:
	addis 10,0,outbuf@ha
	addi 10,10,outbuf@l
	addis 8,0,outcnt@ha
	lwz 11,outcnt@l(8)
	addis 9,0,bi_buf@ha
	lhz 0,bi_buf@l(9)
	srwi 0,0,8
	stbx 0,10,11
	addi 11,11,1
	stw 11,outcnt@l(8)
	cmpwi 1,11,16384
	b .L24
.L16:
	addis 9,0,bi_valid@ha
	lwz 0,bi_valid@l(9)
	cmpwi 1,0,0
	bc 4,5,.L21
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	addis 9,0,bi_buf+1@ha
	lbz 9,bi_buf+1@l(9)
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	cmpwi 1,0,16384
.L24:
	bc 4,6,.L21
	bl flush_outbuf
.L21:
	addis 9,0,bi_buf@ha
	li 0,0
	sth 0,bi_buf@l(9)
	addis 9,0,bi_valid@ha
	li 0,0
	stw 0,bi_valid@l(9)
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe4:
	.size	 bi_windup,.Lfe4-bi_windup
	.align 2
	.globl copy_block
	.type	 copy_block,@function
copy_block:
	stwu 1,-24(1)
	mflr 0
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 30,3
	mr 31,4
	mr 29,5
	bl bi_windup
	cmpwi 1,29,0
	bc 12,6,.L26
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	cmplwi 1,0,16381
	bc 12,5,.L27
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	stbx 31,9,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	rlwinm 11,31,24,24,31
	stbx 11,9,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	b .L28
.L27:
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	addis 11,0,outcnt@ha
	lwz 0,outcnt@l(11)
	stbx 31,9,0
	addic 0,0,1
	stw 0,outcnt@l(11)
	cmpwi 1,0,16384
	bc 4,6,.L29
	bl flush_outbuf
.L29:
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	rlwinm 9,31,24,24,31
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	cmpwi 1,0,16384
	bc 4,6,.L28
	bl flush_outbuf
.L28:
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	cmplwi 1,0,16381
	bc 12,5,.L31
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	nor 9,31,31
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	rlwinm 9,9,24,24,31
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	b .L26
.L31:
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 0,outcnt@l(10)
	nor 9,31,31
	stbx 9,11,0
	addic 0,0,1
	stw 0,outcnt@l(10)
	cmpwi 1,0,16384
	bc 4,6,.L33
	bl flush_outbuf
.L33:
	addis 11,0,outbuf@ha
	addi 11,11,outbuf@l
	addis 10,0,outcnt@ha
	lwz 9,outcnt@l(10)
	nor 0,31,31
	rlwinm 0,0,24,24,31
	stbx 0,11,9
	addi 9,9,1
	stw 9,outcnt@l(10)
	cmpwi 1,9,16384
	bc 4,6,.L26
	bl flush_outbuf
.L26:
	cmpwi 1,31,0
	addi 31,31,-1
	bc 12,6,.L36
	addis 9,0,outbuf@ha
	addi 28,9,outbuf@l
	addis 29,0,outcnt@ha
.L37:
	lwz 0,outcnt@l(29)
	lbz 9,0(30)
	stbx 9,28,0
	addi 30,30,1
	addic 0,0,1
	stw 0,outcnt@l(29)
	cmpwi 1,0,16384
	bc 4,6,.L35
	bl flush_outbuf
.L35:
	cmpwi 1,31,0
	addi 31,31,-1
	bc 4,6,.L37
.L36:
	lwz 0,28(1)
	mtlr 0
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe5:
	.size	 copy_block,.Lfe5-copy_block
	.comm	read_buf,4,4
	.section ".sdata","aw"
	.align 2
zfile:
	.space	4
	.size	 zfile,4
	.align 1
bi_buf:
	.space	2
	.size	 bi_buf,2
	.align 2
bi_valid:
	.space	4
	.size	 bi_valid,4
	.ident	"GCC: (GNU) 2.7-97r2"
