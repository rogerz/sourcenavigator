	.file	"unlzw.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.globl block_mode
	.section ".sdata","aw"
	.align 2
	.type	 block_mode,@object
	.size	 block_mode,4
block_mode:
	.long 128
	.section	".rodata"
	.align 2
.LC0:
	.string	"\n%s: %s: warning, unknown flags 0x%x\n"
	.align 2
.LC1:
	.string	"\n%s: %s: compressed with %d bits, can only handle %d bits\n"
	.align 2
.LC2:
	.string	"corrupt input."
	.align 2
.LC3:
	.string	"corrupt input. Use zcat to recover some data."
	.globl memcpy
	.section ".text"
	.align 2
	.globl unlzw
	.type	 unlzw,@function
unlzw:
	stwu 1,-112(1)
	mflr 0
	stw 14,40(1)
	stw 15,44(1)
	stw 16,48(1)
	stw 17,52(1)
	stw 18,56(1)
	stw 19,60(1)
	stw 20,64(1)
	stw 21,68(1)
	stw 22,72(1)
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
	stw 3,24(1)
	stw 4,28(1)
	addis 9,0,maxbits@ha
	addi 31,9,maxbits@l
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L2
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L3
.L2:
	li 3,0
	bl fill_inbuf
.L3:
	stw 3,0(31)
	addis 11,0,block_mode@ha
	addis 9,0,maxbits@ha
	lwz 9,maxbits@l(9)
	rlwinm 0,9,0,24,24
	stw 0,block_mode@l(11)
	andi. 7,9,96
	bc 12,2,.L4
	addis 9,0,quiet@ha
	lwz 0,quiet@l(9)
	cmpwi 1,0,0
	bc 4,6,.L5
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC0@ha
	addi 4,4,.LC0@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	crxor 6,6,6
	bl fprintf
.L5:
	addis 9,0,exit_code@ha
	lwz 0,exit_code@l(9)
	cmpwi 1,0,0
	bc 4,6,.L4
	li 0,2
	stw 0,exit_code@l(9)
.L4:
	addis 9,0,maxbits@ha
	lwz 0,maxbits@l(9)
	rlwinm 7,0,0,27,31
	stw 7,maxbits@l(9)
	li 31,1
	slw 12,31,7
	stw 12,32(1)
	cmpwi 1,7,16
	bc 4,5,.L7
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,progname@ha
	lwz 3,12(9)
	addis 4,0,.LC1@ha
	addi 4,4,.LC1@l
	lwz 5,progname@l(11)
	addis 6,0,ifname@ha
	addi 6,6,ifname@l
	li 8,16
	crxor 6,6,6
	bl fprintf
	addis 9,0,exit_code@ha
	stw 31,exit_code@l(9)
	mr 3,31
	b .L60
.L7:
	addis 9,0,insize@ha
	lwz 19,insize@l(9)
	li 27,9
	li 20,511
	mr 16,20
	li 25,-1
	li 23,0
	mr 29,23
	addis 9,0,inptr@ha
	lwz 0,inptr@l(9)
	slwi 28,0,3
	addis 9,0,block_mode@ha
	lwz 0,block_mode@l(9)
	srawi 9,0,31
	xor 0,9,0
	subf 0,0,9
	srawi 0,0,31
	andi. 0,0,257
	ori 26,0,256
	addis 3,0,prev@ha
	addi 3,3,prev@l
	mr 4,23
	li 5,256
	bl memset
	li 31,255
	addis 9,0,window@ha
	addi 9,9,window@l
.L13:
	stbx 31,9,31
	addic. 31,31,-1
	bc 4,0,.L13
.L18:
	srawi 9,28,3
	addis 12,0,insize@ha
	lwz 0,insize@l(12)
	subf 4,9,0
	li 10,0
	cmpw 1,10,4
	bc 4,4,.L20
.L22:
	add 0,10,9
	addis 12,0,inbuf@ha
	addi 12,12,inbuf@l
	lbzx 0,12,0
	stbx 0,12,10
	addi 10,10,1
	cmpw 1,10,4
	bc 12,4,.L22
.L20:
	addis 12,0,insize@ha
	stw 4,insize@l(12)
	li 28,0
	cmplwi 1,4,63
	bc 12,5,.L24
	lwz 3,24(1)
	addis 12,0,inbuf@ha
	addi 12,12,inbuf@l
	add 4,4,12
	li 5,0
	ori 5,5,32768
	bl read
	mr 19,3
	cmpwi 1,19,-1
	bc 4,6,.L25
	bl read_error
.L25:
	addis 12,0,insize@ha
	lwz 0,insize@l(12)
	add 0,19,0
	stw 0,insize@l(12)
	addis 9,0,bytes_in@ha
	lwz 0,bytes_in@l(9)
	add 0,19,0
	stw 0,bytes_in@l(9)
.L24:
	cmpwi 1,19,0
	bc 12,6,.L26
	addis 12,0,insize@ha
	lwz 0,insize@l(12)
	divwu 0,0,27
	mullw 0,0,27
	slwi 21,0,3
	b .L27
.L61:
	addis 12,0,prev@ha
	addi 3,12,prev@l
	li 4,0
	li 5,256
	bl memset
	li 26,256
	slwi 9,27,3
	addi 11,9,-1
	add 11,28,11
	divw 0,11,9
	mullw 0,0,9
	subf 11,0,11
	subf 9,11,9
	addi 9,9,-1
	add 28,28,9
	li 27,9
	li 20,511
	mr 16,20
	b .L18
.L26:
	addis 12,0,insize@ha
	lwz 0,insize@l(12)
	slwi 0,0,3
	addic 0,0,1
	subf 21,27,0
.L27:
	cmpw 1,21,28
	bc 4,5,.L17
	addis 9,0,outbuf@ha
	addi 17,9,outbuf@l
	addis 14,0,test@ha
	addis 22,0,bytes_out@ha
	addis 9,0,window@ha
	addi 18,9,window@l
	addis 9,0,d_buf+65534@ha
	addi 15,9,d_buf+65534@l
.L30:
	cmpw 1,26,20
	bc 4,5,.L31
	slwi 11,27,3
	addi 9,11,-1
	add 9,28,9
	divw 0,9,11
	mullw 0,0,11
	subf 9,0,9
	subf 11,9,11
	addi 11,11,-1
	add 28,28,11
	addi 27,27,1
	addis 9,0,maxbits@ha
	lwz 0,maxbits@l(9)
	cmpw 1,27,0
	bc 4,6,.L32
	lwz 20,32(1)
	b .L33
.L32:
	li 12,1
	slw 0,12,27
	addic 20,0,-1
.L33:
	li 12,1
	slw 0,12,27
	addic 16,0,-1
	b .L18
.L31:
	srawi 0,28,3
	addis 12,0,inbuf@ha
	addi 12,12,inbuf@l
	add 11,0,12
	addis 12,0,inbuf@ha
	addi 12,12,inbuf@l
	lbzx 9,12,0
	lbz 0,1(11)
	slwi 0,0,8
	or 9,9,0
	lbz 0,2(11)
	slwi 0,0,16
	or 9,9,0
	rlwinm 0,28,0,29,31
	sraw 9,9,0
	and 31,9,16
	add 28,28,27
	cmpwi 1,25,-1
	bc 4,6,.L34
	cmpwi 1,31,255
	bc 4,5,.L35
	addis 12,0,.LC2@ha
	addi 3,12,.LC2@l
	bl error
.L35:
	mr 25,31
	mr 23,25
	stbx 25,17,29
	addi 29,29,1
	b .L28
.L34:
	cmpwi 1,31,256
	bc 4,6,.L36
	addis 9,0,block_mode@ha
	lwz 0,block_mode@l(9)
	cmpwi 1,0,0
	bc 4,6,.L61
.L36:
	mr 24,31
	addis 12,0,d_buf+65534@ha
	addi 30,12,d_buf+65534@l
	cmpw 1,24,26
	bc 12,4,.L37
	bc 4,5,.L38
	lwz 9,test@l(14)
	subfic 0,9,0
	adde 9,0,9
	srawi 0,29,31
	subf 0,29,0
	srwi 0,0,31
	and. 11,9,0
	bc 12,2,.L39
	lwz 3,28(1)
	addis 12,0,outbuf@ha
	addi 4,12,outbuf@l
	mr 5,29
	bl write_buf
	lwz 0,bytes_out@l(22)
	add 0,29,0
	stw 0,bytes_out@l(22)
.L39:
	addis 9,0,to_stdout@ha
	lwz 0,to_stdout@l(9)
	cmpwi 1,0,0
	bc 12,6,.L40
	addis 12,0,.LC2@ha
	addi 3,12,.LC2@l
	b .L41
.L40:
	addis 9,0,.LC3@ha
	addi 3,9,.LC3@l
.L41:
	bl error
.L38:
	stbu 23,-1(30)
	mr 31,25
.L37:
	cmplwi 1,31,255
	bc 4,5,.L43
	addis 9,0,prev@ha
	addi 9,9,prev@l
.L44:
	lbzx 0,18,31
	stbu 0,-1(30)
	add 0,31,31
	lhzx 31,9,0
	cmplwi 1,31,255
	bc 12,5,.L44
.L43:
	lbzx 23,18,31
	stbu 23,-1(30)
	subf 31,30,15
	add 0,29,31
	cmpwi 1,0,16383
	bc 4,5,.L46
.L47:
	subfic 11,29,16384
	cmpw 1,31,11
	cror 7,6,4
	mfcr 0
	rlwinm 0,0,8,1
	neg 0,0
	and 9,31,0
	andc 0,11,0
	or. 31,9,0
	bc 4,1,.L51
	add 3,29,17
	mr 4,30
	mr 5,31
	crxor 6,6,6
	bl memcpy
	add 29,29,31
.L51:
	cmpwi 1,29,16383
	bc 4,5,.L52
	lwz 0,test@l(14)
	cmpwi 1,0,0
	bc 4,6,.L53
	lwz 3,28(1)
	addis 12,0,outbuf@ha
	addi 4,12,outbuf@l
	mr 5,29
	bl write_buf
	lwz 0,bytes_out@l(22)
	add 0,29,0
	stw 0,bytes_out@l(22)
.L53:
	li 29,0
.L52:
	add 30,30,31
	subf. 31,30,15
	bc 12,1,.L47
	b .L55
.L46:
	add 3,29,17
	mr 4,30
	mr 5,31
	crxor 6,6,6
	bl memcpy
	add 29,29,31
.L55:
	lwz 12,32(1)
	cmpw 1,26,12
	bc 4,4,.L56
	addis 12,0,prev@ha
	addi 9,12,prev@l
	add 0,26,26
	sthx 25,9,0
	stbx 23,18,26
	addi 26,26,1
.L56:
	mr 25,24
.L28:
	cmpw 1,21,28
	bc 12,5,.L30
.L17:
	cmpwi 1,19,0
	bc 4,6,.L18
	addis 9,0,test@ha
	lwz 9,test@l(9)
	subfic 0,9,0
	adde 9,0,9
	srawi 0,29,31
	subf 0,29,0
	srwi 0,0,31
	and. 11,9,0
	bc 12,2,.L59
	lwz 3,28(1)
	addis 4,0,outbuf@ha
	addi 4,4,outbuf@l
	mr 5,29
	bl write_buf
	addis 9,0,bytes_out@ha
	lwz 0,bytes_out@l(9)
	add 0,29,0
	stw 0,bytes_out@l(9)
.L59:
	li 3,0
.L60:
	lwz 0,116(1)
	mtlr 0
	lwz 14,40(1)
	lwz 15,44(1)
	lwz 16,48(1)
	lwz 17,52(1)
	lwz 18,56(1)
	lwz 19,60(1)
	lwz 20,64(1)
	lwz 21,68(1)
	lwz 22,72(1)
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
.Lfe1:
	.size	 unlzw,.Lfe1-unlzw
	.ident	"GCC: (GNU) 2.7-97r2"
