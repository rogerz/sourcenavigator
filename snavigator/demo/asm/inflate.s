	.file	"inflate.c"

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
	.type	 border,@object
border:
	.long 16
	.long 17
	.long 18
	.long 0
	.long 8
	.long 7
	.long 9
	.long 6
	.long 10
	.long 5
	.long 11
	.long 4
	.long 12
	.long 3
	.long 13
	.long 2
	.long 14
	.long 1
	.long 15
	.size	 border,76
	.align 1
	.type	 cplens,@object
cplens:
	.short 3
	.short 4
	.short 5
	.short 6
	.short 7
	.short 8
	.short 9
	.short 10
	.short 11
	.short 13
	.short 15
	.short 17
	.short 19
	.short 23
	.short 27
	.short 31
	.short 35
	.short 43
	.short 51
	.short 59
	.short 67
	.short 83
	.short 99
	.short 115
	.short 131
	.short 163
	.short 195
	.short 227
	.short 258
	.short 0
	.short 0
	.size	 cplens,62
	.align 1
	.type	 cplext,@object
cplext:
	.short 0
	.short 0
	.short 0
	.short 0
	.short 0
	.short 0
	.short 0
	.short 0
	.short 1
	.short 1
	.short 1
	.short 1
	.short 2
	.short 2
	.short 2
	.short 2
	.short 3
	.short 3
	.short 3
	.short 3
	.short 4
	.short 4
	.short 4
	.short 4
	.short 5
	.short 5
	.short 5
	.short 5
	.short 0
	.short 99
	.short 99
	.size	 cplext,62
	.align 1
	.type	 cpdist,@object
cpdist:
	.short 1
	.short 2
	.short 3
	.short 4
	.short 5
	.short 7
	.short 9
	.short 13
	.short 17
	.short 25
	.short 33
	.short 49
	.short 65
	.short 97
	.short 129
	.short 193
	.short 257
	.short 385
	.short 513
	.short 769
	.short 1025
	.short 1537
	.short 2049
	.short 3073
	.short 4097
	.short 6145
	.short 8193
	.short 12289
	.short 16385
	.short 24577
	.size	 cpdist,60
	.align 1
	.type	 cpdext,@object
cpdext:
	.short 0
	.short 0
	.short 0
	.short 0
	.short 1
	.short 1
	.short 2
	.short 2
	.short 3
	.short 3
	.short 4
	.short 4
	.short 5
	.short 5
	.short 6
	.short 6
	.short 7
	.short 7
	.short 8
	.short 8
	.short 9
	.short 9
	.short 10
	.short 10
	.short 11
	.short 11
	.short 12
	.short 12
	.short 13
	.short 13
	.size	 cpdext,60
	.globl mask_bits
	.align 1
	.type	 mask_bits,@object
mask_bits:
	.short 0
	.short 1
	.short 3
	.short 7
	.short 15
	.short 31
	.short 63
	.short 127
	.short 255
	.short 511
	.short 1023
	.short 2047
	.short 4095
	.short 8191
	.short 16383
	.short 32767
	.short 65535
	.size	 mask_bits,34
	.globl lbits
	.section ".sdata","aw"
	.align 2
	.type	 lbits,@object
	.size	 lbits,4
lbits:
	.long 9
	.globl dbits
	.align 2
	.type	 dbits,@object
	.size	 dbits,4
dbits:
	.long 6
	.section ".text"
	.align 2
	.globl huft_build
	.type	 huft_build,@function
huft_build:
	stwu 1,-1456(1)
	mflr 0
	stw 14,1384(1)
	stw 15,1388(1)
	stw 16,1392(1)
	stw 17,1396(1)
	stw 18,1400(1)
	stw 19,1404(1)
	stw 20,1408(1)
	stw 21,1412(1)
	stw 22,1416(1)
	stw 23,1420(1)
	stw 24,1424(1)
	stw 25,1428(1)
	stw 26,1432(1)
	stw 27,1436(1)
	stw 28,1440(1)
	stw 29,1444(1)
	stw 30,1448(1)
	stw 31,1452(1)
	stw 0,1460(1)
	mr 26,3
	mr 14,4
	stw 5,1368(1)
	stw 6,1372(1)
	stw 7,1376(1)
	mr 16,8
	mr 27,9
	addi 3,1,8
	li 4,0
	li 5,68
	bl memset
	mr 24,26
	mr 28,14
	addi 11,1,8
.L2:
	lwz 9,0(24)
	slwi 9,9,2
	lwzx 0,11,9
	addic 0,0,1
	stwx 0,11,9
	addi 24,24,4
	addic. 28,28,-1
	bc 4,2,.L2
	lwz 0,8(1)
	cmpw 1,0,14
	bc 4,6,.L6
	li 0,0
	stw 0,0(16)
	stw 0,0(27)
	mr 3,0
	b .L81
.L6:
	lwz 23,0(27)
	li 31,1
	addi 9,1,8
.L10:
	slwi 0,31,2
	lwzx 0,9,0
	cmpwi 1,0,0
	bc 4,6,.L8
	addi 31,31,1
	cmplwi 1,31,16
	bc 4,5,.L10
.L8:
	mr 22,31
	subfc 9,22,23
	subfe 9,9,9
	nand 9,9,9
	and 0,23,9
	andc 9,22,9
	or 23,0,9
	li 28,16
	addi 9,1,8
.L17:
	slwi 0,28,2
	lwzx 0,9,0
	cmpwi 1,0,0
	bc 4,6,.L15
	addic. 28,28,-1
	bc 4,2,.L17
.L15:
	mr 17,28
	subfc 9,23,17
	subfe 9,9,9
	nand 9,9,9
	and 0,23,9
	andc 9,17,9
	or 23,0,9
	stw 23,0(27)
	li 0,1
	slw 19,0,31
	cmplw 1,31,17
	bc 4,4,.L22
	addi 9,1,8
.L24:
	slwi 0,31,2
	lwzx 0,9,0
	subf. 19,0,19
	bc 12,0,.L83
	addi 31,31,1
	slwi 19,19,1
	cmplw 1,31,28
	bc 12,4,.L24
.L22:
	slwi 0,28,2
	addi 9,1,8
	lwzx 0,9,0
	subf. 19,0,19
	bc 4,0,.L27
.L83:
	li 3,2
	b .L81
.L27:
	slwi 11,28,2
	addi 9,1,8
	lwzx 0,9,11
	add 0,19,0
	stwx 0,9,11
	li 31,0
	stw 31,1300(1)
	addi 24,1,12
	addi 9,1,1304
	addic. 28,28,-1
	bc 12,2,.L29
.L30:
	lwz 0,0(24)
	add 31,31,0
	stw 31,0(9)
	addi 24,24,4
	addi 9,9,4
	addic. 28,28,-1
	bc 4,2,.L30
.L29:
	mr 24,26
	li 28,0
	addi 10,1,8
.L32:
	lwz 31,0(24)
	addi 24,24,4
	cmpwi 1,31,0
	bc 12,6,.L34
	slwi 11,31,2
	add 11,10,11
	lwz 0,1288(11)
	slwi 9,0,2
	add 9,10,9
	stw 28,136(9)
	addic 0,0,1
	stw 0,1288(11)
.L34:
	addi 28,28,1
	cmplw 1,28,14
	bc 12,4,.L32
	li 28,0
	stw 28,1296(1)
	addi 24,1,144
	li 25,-1
	neg 27,23
	stw 28,80(1)
	mr 3,28
	mr 26,28
	cmpw 1,22,17
	bc 12,5,.L38
	addi 20,1,8
	li 18,1
	addis 15,0,hufts@ha
.L40:
	slwi 0,22,2
	lwzx 21,20,0
	cmpwi 1,21,0
	addi 21,21,-1
	bc 12,6,.L39
	mr 12,0
	stw 12,1380(1)
.L43:
	add 0,27,23
	mr 9,0
	cmpw 1,22,0
	bc 4,5,.L45
.L46:
	addi 25,25,1
	mr 27,9
	subf 26,27,17
	subfc 9,26,23
	subfe 9,9,9
	nand 9,9,9
	and 0,26,9
	andc 3,23,9
	or 26,0,3
	subf 31,27,22
	slw 11,18,31
	addi 0,21,1
	cmplw 1,11,0
	bc 4,5,.L49
	addi 0,11,-1
	subf 11,21,0
	lwz 12,1380(1)
	add 9,20,12
	b .L84
.L52:
	slwi 11,11,1
	lwzu 0,4(9)
	cmplw 1,11,0
	bc 4,5,.L49
	subf 11,0,11
.L84:
	addi 31,31,1
	cmplw 1,31,26
	bc 12,4,.L52
.L49:
	slw 26,18,31
	addi 3,26,1
	slwi 3,3,3
	bl malloc
	mr. 3,3
	bc 4,2,.L55
	cmpwi 1,25,0
	bc 12,6,.L56
	lwz 3,80(1)
	bl huft_free
.L56:
	li 3,3
	b .L81
.L55:
	lwz 0,hufts@l(15)
	addic 0,0,1
	add 0,0,26
	stw 0,hufts@l(15)
	addi 9,3,8
	stw 9,0(16)
	addi 16,3,4
	li 0,0
	stw 0,4(3)
	slwi 0,25,2
	add 11,20,0
	mr 3,9
	stw 3,72(11)
	cmpwi 1,25,0
	bc 12,6,.L44
	stw 28,1288(11)
	rlwimi 29,23,16,8,15
	addi 0,31,16
	rlwimi 29,0,24,0,7
	mr 30,3
	subf 0,23,27
	srw 31,28,0
	addi 9,25,-1
	slwi 9,9,2
	add 9,20,9
	lwz 0,72(9)
	slwi 9,31,3
	add 9,9,0
	stw 29,0(9)
	stw 30,4(9)
.L44:
	add 9,27,23
	cmpw 1,22,9
	bc 12,5,.L46
.L45:
	subf 0,27,22
	rlwimi 29,0,16,8,15
	addi 0,1,144
	slwi 9,14,2
	add 0,0,9
	cmplw 1,24,0
	bc 12,4,.L59
	li 0,99
	rlwimi 29,0,24,0,7
	b .L60
.L59:
	lwz 0,0(24)
	lwz 12,1368(1)
	cmplw 1,0,12
	bc 4,4,.L61
	subfic 9,0,255
	subfe 9,9,9
	rlwinm 0,9,0,28,31
	nor 9,9,9
	rlwinm 9,9,0,27,27
	or 0,0,9
	rlwimi 29,0,24,0,7
	lhz 0,2(24)
	b .L85
.L61:
	lwz 0,0(24)
	lwz 12,1368(1)
	subf 0,12,0
	add 0,0,0
	lwz 12,1376(1)
	add 9,0,12
	lbz 9,1(9)
	rlwimi 29,9,24,0,7
	lwz 12,1372(1)
	lhzx 0,12,0
.L85:
	rlwimi 30,0,16,0,15
	addi 24,24,4
.L60:
	subf 0,27,22
	slw 11,18,0
	srw 31,28,27
	cmplw 1,31,26
	bc 4,4,.L66
.L68:
	slwi 9,31,3
	add 9,9,3
	stw 29,0(9)
	stw 30,4(9)
	add 31,31,11
	cmplw 1,31,26
	bc 12,4,.L68
.L66:
	addi 0,22,-1
	slw 31,18,0
	and. 0,28,31
	bc 12,2,.L71
.L73:
	xor 28,28,31
	srwi 31,31,1
	and. 0,28,31
	bc 4,2,.L73
.L71:
	xor 28,28,31
	slw 11,18,27
	addi 11,11,-1
	and 11,28,11
	slwi 9,25,2
	mr 10,20
	add 9,20,9
	lwz 0,1288(9)
	cmpw 1,11,0
	bc 12,6,.L41
	li 8,1
.L77:
	addi 25,25,-1
	subf 27,23,27
	slw 11,8,27
	addi 11,11,-1
	and 11,28,11
	slwi 9,25,2
	add 9,10,9
	lwz 0,1288(9)
	cmpw 1,11,0
	bc 4,6,.L77
.L41:
	cmpwi 1,21,0
	addi 21,21,-1
	bc 4,6,.L43
.L39:
	addi 22,22,1
	cmpw 1,22,17
	bc 4,5,.L40
.L38:
	addic 0,19,-1
	subfe 9,0,19
	xori 3,17,1
	addic 0,3,-1
	subfe 3,0,3
	and 3,9,3
.L81:
	lwz 0,1460(1)
	mtlr 0
	lwz 14,1384(1)
	lwz 15,1388(1)
	lwz 16,1392(1)
	lwz 17,1396(1)
	lwz 18,1400(1)
	lwz 19,1404(1)
	lwz 20,1408(1)
	lwz 21,1412(1)
	lwz 22,1416(1)
	lwz 23,1420(1)
	lwz 24,1424(1)
	lwz 25,1428(1)
	lwz 26,1432(1)
	lwz 27,1436(1)
	lwz 28,1440(1)
	lwz 29,1444(1)
	lwz 30,1448(1)
	lwz 31,1452(1)
	addi 1,1,1456
	blr
.Lfe1:
	.size	 huft_build,.Lfe1-huft_build
	.align 2
	.globl huft_free
	.type	 huft_free,@function
huft_free:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr. 3,3
	bc 12,2,.L88
.L89:
	addi 3,3,-8
	lwz 29,4(3)
	bl free
	mr. 3,29
	bc 4,2,.L89
.L88:
	li 3,0
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe2:
	.size	 huft_free,.Lfe2-huft_free
	.globl memcpy
	.align 2
	.globl inflate_codes
	.type	 inflate_codes,@function
inflate_codes:
	stwu 1,-104(1)
	mflr 0
	stw 14,32(1)
	stw 15,36(1)
	stw 16,40(1)
	stw 17,44(1)
	stw 18,48(1)
	stw 19,52(1)
	stw 20,56(1)
	stw 21,60(1)
	stw 22,64(1)
	stw 23,68(1)
	stw 24,72(1)
	stw 25,76(1)
	stw 26,80(1)
	stw 27,84(1)
	stw 28,88(1)
	stw 29,92(1)
	stw 30,96(1)
	stw 31,100(1)
	stw 0,108(1)
	mr 16,3
	mr 17,4
	mr 20,5
	mr 21,6
	addis 9,0,bb@ha
	lwz 28,bb@l(9)
	addis 9,0,bk@ha
	lwz 29,bk@l(9)
	addis 9,0,outcnt@ha
	lwz 27,outcnt@l(9)
	addis 9,0,mask_bits@ha
	addi 9,9,mask_bits@l
	add 0,20,20
	lhzx 0,9,0
	stw 0,24(1)
	add 0,21,21
	lhzx 14,9,0
	addis 25,0,inptr@ha
	addis 23,0,insize@ha
	addis 9,0,inbuf@ha
	addi 24,9,inbuf@l
	addis 9,0,mask_bits@ha
	addi 18,9,mask_bits@l
	addis 9,0,window@ha
	addi 22,9,window@l
	li 19,0
	ori 19,19,32768
	addis 15,0,outcnt@ha
	b .L163
.L97:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L98
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L99
.L98:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L99:
	addi 29,29,8
.L163:
	cmplw 1,29,20
	bc 12,4,.L97
	lwz 10,24(1)
	and 0,28,10
	slwi 0,0,3
	add 30,16,0
	lbzx 31,16,0
	cmplwi 1,31,16
	bc 4,5,.L101
.L102:
	cmpwi 1,31,99
	bc 12,6,.L164
	lbz 0,1(30)
	srw 28,28,0
	subf 29,0,29
	addi 31,31,-16
	cmplw 1,29,31
	bc 4,4,.L104
.L108:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L109
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L110
.L109:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L110:
	addi 29,29,8
	cmplw 1,29,31
	bc 12,4,.L108
.L104:
	add 0,31,31
	lhzx 9,18,0
	and 9,28,9
	slwi 9,9,3
	lwz 11,4(30)
	add 30,9,11
	lbzx 31,9,11
	cmplwi 1,31,16
	bc 12,5,.L102
.L101:
	lbz 0,1(30)
	srw 28,28,0
	subf 29,0,29
	cmpwi 1,31,16
	bc 4,6,.L113
	lbz 0,5(30)
	stbx 0,22,27
	addi 27,27,1
	cmpw 1,27,19
	bc 4,6,.L163
	stw 27,outcnt@l(15)
	bl flush_window
	li 27,0
	b .L163
.L113:
	cmpwi 1,31,15
	bc 12,6,.L93
	cmplw 1,29,31
	bc 4,4,.L118
.L119:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L120
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L121
.L120:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L121:
	addi 29,29,8
	cmplw 1,29,31
	bc 12,4,.L119
.L118:
	lhz 26,4(30)
	add 0,31,31
	lhzx 0,18,0
	and 0,28,0
	add 26,26,0
	srw 28,28,31
	subf 29,31,29
	cmplw 1,29,21
	bc 4,4,.L124
.L125:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L126
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L127
.L126:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L127:
	addi 29,29,8
	cmplw 1,29,21
	bc 12,4,.L125
.L124:
	and 0,28,14
	slwi 0,0,3
	add 30,17,0
	lbzx 31,17,0
	cmplwi 1,31,16
	bc 4,5,.L129
.L130:
	cmpwi 1,31,99
	bc 4,6,.L133
.L164:
	li 3,1
	b .L162
.L133:
	lbz 0,1(30)
	srw 28,28,0
	subf 29,0,29
	addi 31,31,-16
	cmplw 1,29,31
	bc 4,4,.L132
.L136:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L137
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L138
.L137:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L138:
	addi 29,29,8
	cmplw 1,29,31
	bc 12,4,.L136
.L132:
	add 0,31,31
	lhzx 9,18,0
	and 9,28,9
	slwi 9,9,3
	lwz 11,4(30)
	add 30,9,11
	lbzx 31,9,11
	cmplwi 1,31,16
	bc 12,5,.L130
.L129:
	lbz 0,1(30)
	srw 28,28,0
	subf 29,0,29
	cmplw 1,29,31
	bc 4,4,.L142
.L143:
	lwz 9,inptr@l(25)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L144
	lbzx 0,24,9
	slw 0,0,29
	or 28,28,0
	addi 0,9,1
	stw 0,inptr@l(25)
	b .L145
.L144:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,29
	or 28,28,3
.L145:
	addi 29,29,8
	cmplw 1,29,31
	bc 12,4,.L143
.L142:
	lhz 0,4(30)
	subf 30,0,27
	add 0,31,31
	lhzx 0,18,0
	and 0,28,0
	subf 30,0,30
	srw 28,28,31
	subf 29,31,29
.L147:
	rlwinm 30,30,0,17,31
	cmplw 1,30,27
	bc 4,5,.L152
	subf 31,30,19
	b .L153
.L152:
	subf 31,27,19
.L153:
	subfc 9,31,26
	subfe 9,9,9
	nand 9,9,9
	and 0,31,9
	andc 5,26,9
	or 31,0,5
	subf 26,31,26
	subf 0,30,27
	cmplw 1,0,31
	bc 12,4,.L156
	add 3,27,22
	add 4,30,22
	mr 5,31
	crxor 6,6,6
	bl memcpy
	add 27,27,31
	add 30,30,31
	b .L155
.L156:
	lbzx 0,22,30
	stbx 0,22,27
	addi 30,30,1
	addi 27,27,1
	addic. 31,31,-1
	bc 4,2,.L156
.L155:
	cmpw 1,27,19
	bc 4,6,.L149
	stw 27,outcnt@l(15)
	bl flush_window
	li 27,0
.L149:
	cmpwi 1,26,0
	bc 4,6,.L147
	b .L163
.L93:
	addis 9,0,outcnt@ha
	stw 27,outcnt@l(9)
	addis 9,0,bb@ha
	stw 28,bb@l(9)
	addis 9,0,bk@ha
	stw 29,bk@l(9)
	li 3,0
.L162:
	lwz 0,108(1)
	mtlr 0
	lwz 14,32(1)
	lwz 15,36(1)
	lwz 16,40(1)
	lwz 17,44(1)
	lwz 18,48(1)
	lwz 19,52(1)
	lwz 20,56(1)
	lwz 21,60(1)
	lwz 22,64(1)
	lwz 23,68(1)
	lwz 24,72(1)
	lwz 25,76(1)
	lwz 26,80(1)
	lwz 27,84(1)
	lwz 28,88(1)
	lwz 29,92(1)
	lwz 30,96(1)
	lwz 31,100(1)
	addi 1,1,104
	blr
.Lfe3:
	.size	 inflate_codes,.Lfe3-inflate_codes
	.align 2
	.globl inflate_stored
	.type	 inflate_stored,@function
inflate_stored:
	stwu 1,-40(1)
	mflr 0
	stw 25,12(1)
	stw 26,16(1)
	stw 27,20(1)
	stw 28,24(1)
	stw 29,28(1)
	stw 30,32(1)
	stw 31,36(1)
	stw 0,44(1)
	addis 9,0,bb@ha
	lwz 30,bb@l(9)
	addis 9,0,bk@ha
	lwz 31,bk@l(9)
	addis 9,0,outcnt@ha
	lwz 28,outcnt@l(9)
	rlwinm 29,31,0,29,31
	srw 30,30,29
	subf 31,29,31
	cmplwi 1,31,15
	bc 12,5,.L167
	addis 29,0,inptr@ha
	addis 26,0,insize@ha
	addis 9,0,inbuf@ha
	addi 27,9,inbuf@l
.L168:
	lwz 9,inptr@l(29)
	lwz 0,insize@l(26)
	cmplw 1,9,0
	bc 4,4,.L169
	lbzx 0,27,9
	slw 0,0,31
	or 30,30,0
	addi 0,9,1
	stw 0,inptr@l(29)
	b .L170
.L169:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,31
	or 30,30,3
.L170:
	addi 31,31,8
	cmplwi 1,31,15
	bc 4,5,.L168
.L167:
	rlwinm 29,30,0,16,31
	srwi 30,30,16
	addi 31,31,-16
	cmplwi 1,31,15
	bc 12,5,.L173
	addis 27,0,inptr@ha
	addis 25,0,insize@ha
	addis 9,0,inbuf@ha
	addi 26,9,inbuf@l
.L174:
	lwz 9,inptr@l(27)
	lwz 0,insize@l(25)
	cmplw 1,9,0
	bc 4,4,.L175
	lbzx 0,26,9
	slw 0,0,31
	or 30,30,0
	addi 0,9,1
	stw 0,inptr@l(27)
	b .L176
.L175:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,31
	or 30,30,3
.L176:
	addi 31,31,8
	cmplwi 1,31,15
	bc 4,5,.L174
.L173:
	nor 0,30,30
	rlwinm 0,0,0,16,31
	cmpw 1,29,0
	bc 12,6,.L178
	li 3,1
	b .L190
.L178:
	srwi 30,30,16
	addi 31,31,-16
	cmpwi 1,29,0
	addi 29,29,-1
	bc 12,6,.L180
	addis 27,0,inptr@ha
	addis 25,0,insize@ha
	addis 9,0,inbuf@ha
	addi 26,9,inbuf@l
	b .L191
.L184:
	lwz 9,inptr@l(27)
	lwz 0,insize@l(25)
	cmplw 1,9,0
	bc 4,4,.L185
	lbzx 0,26,9
	slw 0,0,31
	or 30,30,0
	addi 0,9,1
	stw 0,inptr@l(27)
	b .L186
.L185:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,31
	or 30,30,3
.L186:
	addi 31,31,8
.L191:
	cmplwi 1,31,7
	bc 4,5,.L184
	addis 9,0,window@ha
	addi 9,9,window@l
	stbx 30,9,28
	cmpwi 1,28,32767
	addi 28,28,1
	bc 4,6,.L188
	addis 9,0,outcnt@ha
	stw 28,outcnt@l(9)
	bl flush_window
	li 28,0
.L188:
	srwi 30,30,8
	addi 31,31,-8
	cmpwi 1,29,0
	addi 29,29,-1
	bc 4,6,.L191
.L180:
	addis 9,0,outcnt@ha
	stw 28,outcnt@l(9)
	addis 9,0,bb@ha
	stw 30,bb@l(9)
	addis 9,0,bk@ha
	stw 31,bk@l(9)
	li 3,0
.L190:
	lwz 0,44(1)
	mtlr 0
	lwz 25,12(1)
	lwz 26,16(1)
	lwz 27,20(1)
	lwz 28,24(1)
	lwz 29,28(1)
	lwz 30,32(1)
	lwz 31,36(1)
	addi 1,1,40
	blr
.Lfe4:
	.size	 inflate_stored,.Lfe4-inflate_stored
	.align 2
	.globl inflate_fixed
	.type	 inflate_fixed,@function
inflate_fixed:
	stwu 1,-1184(1)
	mflr 0
	stw 31,1180(1)
	stw 0,1188(1)
	li 31,0
	li 11,8
	add 9,1,11
.L196:
	slwi 0,31,2
	stwx 11,9,0
	addi 31,31,1
	cmpwi 1,31,143
	bc 4,5,.L196
	cmpwi 1,31,255
	bc 12,5,.L222
	li 11,9
	addi 9,1,8
.L201:
	slwi 0,31,2
	stwx 11,9,0
	addi 31,31,1
	cmpwi 1,31,255
	bc 4,5,.L201
.L222:
	cmpwi 1,31,279
	bc 12,5,.L223
	li 11,7
	addi 9,1,8
.L206:
	slwi 0,31,2
	stwx 11,9,0
	addi 31,31,1
	cmpwi 1,31,279
	bc 4,5,.L206
.L223:
	cmpwi 1,31,287
	bc 12,5,.L224
	li 11,8
	add 9,1,11
.L211:
	slwi 0,31,2
	stwx 11,9,0
	addi 31,31,1
	cmpwi 1,31,287
	bc 4,5,.L211
.L224:
	li 0,7
	stw 0,1164(1)
	addi 3,1,8
	li 4,288
	li 5,257
	addis 6,0,cplens@ha
	addi 6,6,cplens@l
	addis 7,0,cplext@ha
	addi 7,7,cplext@l
	addi 8,1,1160
	addi 9,1,1164
	bl huft_build
	mr. 31,3
	bc 4,2,.L225
	li 31,0
	li 11,5
	addi 9,1,8
.L217:
	slwi 0,31,2
	stwx 11,9,0
	addi 31,31,1
	cmpwi 1,31,29
	bc 4,5,.L217
	li 0,5
	stw 0,1172(1)
	addi 3,1,8
	li 4,30
	li 5,0
	addis 6,0,cpdist@ha
	addi 6,6,cpdist@l
	addis 7,0,cpdext@ha
	addi 7,7,cpdext@l
	addi 8,1,1168
	addi 9,1,1172
	bl huft_build
	mr 31,3
	cmpwi 1,31,1
	bc 12,5,.L219
	lwz 3,1160(1)
	lwz 4,1168(1)
	lwz 5,1164(1)
	lwz 6,1172(1)
	bl inflate_codes
	cmpwi 1,3,0
	li 3,1
	bc 4,6,.L221
	lwz 3,1160(1)
	bl huft_free
	lwz 3,1168(1)
	bl huft_free
	li 3,0
	b .L221
.L219:
	lwz 3,1160(1)
	bl huft_free
.L225:
	mr 3,31
.L221:
	lwz 0,1188(1)
	mtlr 0
	lwz 31,1180(1)
	addi 1,1,1184
	blr
.Lfe5:
	.size	 inflate_fixed,.Lfe5-inflate_fixed
	.section	".rodata"
	.align 2
.LC0:
	.string	" incomplete literal tree\n"
	.align 2
.LC1:
	.string	" incomplete distance tree\n"
	.section ".text"
	.align 2
	.globl inflate_dynamic
	.type	 inflate_dynamic,@function
inflate_dynamic:
	stwu 1,-1344(1)
	mflr 0
	stw 19,1292(1)
	stw 20,1296(1)
	stw 21,1300(1)
	stw 22,1304(1)
	stw 23,1308(1)
	stw 24,1312(1)
	stw 25,1316(1)
	stw 26,1320(1)
	stw 27,1324(1)
	stw 28,1328(1)
	stw 29,1332(1)
	stw 30,1336(1)
	stw 31,1340(1)
	stw 0,1348(1)
	addis 9,0,bb@ha
	lwz 29,bb@l(9)
	addis 9,0,bk@ha
	lwz 30,bk@l(9)
	cmplwi 1,30,4
	bc 12,5,.L228
	addis 31,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L229:
	lwz 9,inptr@l(31)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L230
	lbzx 0,28,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(31)
	b .L231
.L230:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L231:
	addi 30,30,8
	cmplwi 1,30,4
	bc 4,5,.L229
.L228:
	rlwinm 21,29,0,27,31
	addi 21,21,257
	srwi 29,29,5
	addi 30,30,-5
	cmplwi 1,30,4
	bc 12,5,.L234
	addis 31,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L235:
	lwz 9,inptr@l(31)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L236
	lbzx 0,28,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(31)
	b .L237
.L236:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L237:
	addi 30,30,8
	cmplwi 1,30,4
	bc 4,5,.L235
.L234:
	rlwinm 20,29,0,27,31
	addi 20,20,1
	srwi 29,29,5
	addi 30,30,-5
	cmplwi 1,30,3
	bc 12,5,.L240
	addis 31,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L241:
	lwz 9,inptr@l(31)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L242
	lbzx 0,28,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(31)
	b .L243
.L242:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L243:
	addi 30,30,8
	cmplwi 1,30,3
	bc 4,5,.L241
.L240:
	rlwinm 28,29,0,28,31
	addi 28,28,4
	srwi 29,29,4
	addi 30,30,-4
	subfic 0,21,286
	subfe 0,0,0
	neg 0,0
	subfic 9,20,30
	subfe 9,9,9
	neg 9,9
	or. 11,0,9
	bc 12,2,.L245
.L327:
	li 3,1
	b .L318
.L245:
	li 31,0
	cmplw 1,31,28
	bc 4,4,.L247
	addis 27,0,inptr@ha
	addis 25,0,insize@ha
	addis 9,0,inbuf@ha
	addi 26,9,inbuf@l
	b .L323
.L252:
	lwz 9,inptr@l(27)
	lwz 0,insize@l(25)
	cmplw 1,9,0
	bc 4,4,.L253
	lbzx 0,26,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(27)
	b .L254
.L253:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L254:
	addi 30,30,8
.L323:
	cmplwi 1,30,2
	bc 4,5,.L252
	addis 9,0,border@ha
	addi 9,9,border@l
	slwi 0,31,2
	lwzx 0,9,0
	slwi 0,0,2
	rlwinm 11,29,0,29,31
	addi 9,1,8
	stwx 11,9,0
	srwi 29,29,3
	addi 30,30,-3
	addi 31,31,1
	cmplw 1,31,28
	bc 12,4,.L323
.L247:
	cmplwi 1,31,18
	bc 12,5,.L319
	addis 9,0,border@ha
	addi 9,9,border@l
	li 10,0
	addi 11,1,8
.L260:
	slwi 0,31,2
	lwzx 0,9,0
	slwi 0,0,2
	stwx 10,11,0
	addi 31,31,1
	cmplwi 1,31,18
	bc 4,5,.L260
.L319:
	li 0,7
	stw 0,1276(1)
	addi 3,1,8
	li 4,19
	mr 5,4
	li 6,0
	mr 7,6
	addi 8,1,1272
	addi 9,1,1276
	bl huft_build
	mr. 28,3
	bc 12,2,.L262
	cmpwi 1,28,1
	bc 4,6,.L324
	b .L316
.L262:
	add 24,21,20
	addis 9,0,mask_bits@ha
	addi 9,9,mask_bits@l
	lwz 0,1276(1)
	add 0,0,0
	lhzx 19,9,0
	li 27,0
	mr 28,27
	cmplw 1,28,24
	bc 4,4,.L265
	addis 26,0,inptr@ha
	addis 22,0,insize@ha
	addis 9,0,inbuf@ha
	addi 23,9,inbuf@l
	addi 25,1,8
	b .L325
.L269:
	lwz 9,inptr@l(26)
	lwz 0,insize@l(22)
	cmplw 1,9,0
	bc 4,4,.L270
	lbzx 0,23,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L271
.L270:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L271:
	addi 30,30,8
.L325:
	lwz 0,1276(1)
	cmplw 1,30,0
	bc 12,4,.L269
	and 9,29,19
	slwi 9,9,3
	lwz 0,1272(1)
	add 9,9,0
	stw 9,1280(1)
	lbz 31,1(9)
	srw 29,29,31
	subf 30,31,30
	lhz 31,4(9)
	cmplwi 1,31,15
	bc 12,5,.L273
	slwi 0,28,2
	mr 27,31
	stwx 27,25,0
	addi 28,28,1
	b .L264
.L273:
	cmpwi 1,31,16
	bc 4,6,.L275
	cmplwi 1,30,1
	bc 12,5,.L277
.L278:
	lwz 9,inptr@l(26)
	lwz 0,insize@l(22)
	cmplw 1,9,0
	bc 4,4,.L279
	lbzx 0,23,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L280
.L279:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L280:
	addi 30,30,8
	cmplwi 1,30,1
	bc 4,5,.L278
.L277:
	rlwinm 0,29,0,30,31
	addic 31,0,3
	srwi 29,29,2
	addi 30,30,-2
	add 0,28,31
	cmplw 1,0,24
	bc 12,5,.L327
	cmpwi 1,31,0
	addi 31,31,-1
	bc 12,6,.L264
.L285:
	slwi 0,28,2
	stwx 27,25,0
	addi 28,28,1
	cmpwi 1,31,0
	addi 31,31,-1
	bc 4,6,.L285
	b .L264
.L275:
	cmpwi 1,31,17
	bc 4,6,.L326
	cmplwi 1,30,2
	bc 12,5,.L290
.L291:
	lwz 9,inptr@l(26)
	lwz 0,insize@l(22)
	cmplw 1,9,0
	bc 4,4,.L292
	lbzx 0,23,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L293
.L292:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L293:
	addi 30,30,8
	cmplwi 1,30,2
	bc 4,5,.L291
.L290:
	rlwinm 31,29,0,29,31
	addi 31,31,3
	srwi 29,29,3
	addi 30,30,-3
	add 0,28,31
	cmplw 1,0,24
	bc 12,5,.L327
	cmpwi 1,31,0
	addi 31,31,-1
	bc 12,6,.L309
	li 9,0
.L298:
	slwi 0,28,2
	stwx 9,25,0
	addi 28,28,1
	cmpwi 1,31,0
	addi 31,31,-1
	bc 4,6,.L298
	b .L309
.L303:
	lwz 9,inptr@l(26)
	lwz 0,insize@l(22)
	cmplw 1,9,0
	bc 4,4,.L304
	lbzx 0,23,9
	slw 0,0,30
	or 29,29,0
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L305
.L304:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,30
	or 29,29,3
.L305:
	addi 30,30,8
.L326:
	cmplwi 1,30,6
	bc 4,5,.L303
	rlwinm 31,29,0,25,31
	addi 31,31,11
	srwi 29,29,7
	addi 30,30,-7
	add 0,28,31
	cmplw 1,0,24
	bc 12,5,.L327
	cmpwi 1,31,0
	addi 31,31,-1
	bc 12,6,.L309
	li 9,0
.L310:
	slwi 0,28,2
	stwx 9,25,0
	addi 28,28,1
	cmpwi 1,31,0
	addi 31,31,-1
	bc 4,6,.L310
.L309:
	li 27,0
.L264:
	cmplw 1,28,24
	bc 12,4,.L325
.L265:
	lwz 3,1272(1)
	bl huft_free
	addis 9,0,bb@ha
	stw 29,bb@l(9)
	addis 9,0,bk@ha
	stw 30,bk@l(9)
	addis 9,0,lbits@ha
	lwz 0,lbits@l(9)
	stw 0,1276(1)
	addi 3,1,8
	mr 4,21
	li 5,257
	addis 6,0,cplens@ha
	addi 6,6,cplens@l
	addis 7,0,cplext@ha
	addi 7,7,cplext@l
	addi 8,1,1272
	addi 9,1,1276
	bl huft_build
	mr. 28,3
	bc 12,2,.L313
	cmpwi 1,28,1
	bc 4,6,.L324
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC0@ha
	addi 4,4,.LC0@l
	crxor 6,6,6
	bl fprintf
	b .L316
.L313:
	addis 9,0,dbits@ha
	lwz 0,dbits@l(9)
	stw 0,1284(1)
	slwi 3,21,2
	addi 0,1,8
	add 3,0,3
	mr 4,20
	li 5,0
	addis 6,0,cpdist@ha
	addi 6,6,cpdist@l
	addis 7,0,cpdext@ha
	addi 7,7,cpdext@l
	addi 8,1,1280
	addi 9,1,1284
	bl huft_build
	mr. 28,3
	bc 12,2,.L315
	cmpwi 1,28,1
	bc 4,6,.L316
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC1@ha
	addi 4,4,.LC1@l
	crxor 6,6,6
	bl fprintf
	lwz 3,1280(1)
	bl huft_free
.L316:
	lwz 3,1272(1)
	bl huft_free
.L324:
	mr 3,28
	b .L318
.L315:
	lwz 3,1272(1)
	lwz 4,1280(1)
	lwz 5,1276(1)
	lwz 6,1284(1)
	bl inflate_codes
	cmpwi 1,3,0
	li 3,1
	bc 4,6,.L318
	lwz 3,1272(1)
	bl huft_free
	lwz 3,1280(1)
	bl huft_free
	li 3,0
.L318:
	lwz 0,1348(1)
	mtlr 0
	lwz 19,1292(1)
	lwz 20,1296(1)
	lwz 21,1300(1)
	lwz 22,1304(1)
	lwz 23,1308(1)
	lwz 24,1312(1)
	lwz 25,1316(1)
	lwz 26,1320(1)
	lwz 27,1324(1)
	lwz 28,1328(1)
	lwz 29,1332(1)
	lwz 30,1336(1)
	lwz 31,1340(1)
	addi 1,1,1344
	blr
.Lfe6:
	.size	 inflate_dynamic,.Lfe6-inflate_dynamic
	.align 2
	.globl inflate_block
	.type	 inflate_block,@function
inflate_block:
	stwu 1,-32(1)
	mflr 0
	stw 26,8(1)
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 26,3
	addis 9,0,bb@ha
	lwz 30,bb@l(9)
	addis 9,0,bk@ha
	lwz 31,bk@l(9)
	cmpwi 1,31,0
	bc 4,6,.L330
	addis 29,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L331:
	lwz 9,inptr@l(29)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L332
	lbzx 0,28,9
	slw 0,0,31
	or 30,30,0
	addi 0,9,1
	stw 0,inptr@l(29)
	b .L333
.L332:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,31
	or 30,30,3
.L333:
	addic. 31,31,8
	bc 12,2,.L331
.L330:
	rlwinm 0,30,0,31,31
	stw 0,0(26)
	srwi 30,30,1
	addi 31,31,-1
	cmplwi 1,31,1
	bc 12,5,.L336
	addis 29,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L337:
	lwz 9,inptr@l(29)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L338
	lbzx 0,28,9
	slw 0,0,31
	or 30,30,0
	addi 0,9,1
	stw 0,inptr@l(29)
	b .L339
.L338:
	li 3,0
	bl fill_inbuf
	rlwinm 3,3,0,0xff
	slw 3,3,31
	or 30,30,3
.L339:
	addi 31,31,8
	cmplwi 1,31,1
	bc 4,5,.L337
.L336:
	rlwinm 0,30,0,30,31
	srwi 30,30,2
	addi 31,31,-2
	addis 9,0,bb@ha
	stw 30,bb@l(9)
	addis 9,0,bk@ha
	stw 31,bk@l(9)
	cmpwi 1,0,2
	bc 4,6,.L341
	bl inflate_dynamic
	b .L344
.L341:
	cmpwi 1,0,0
	bc 4,6,.L342
	bl inflate_stored
	b .L344
.L342:
	cmpwi 1,0,1
	bc 12,6,.L343
	li 3,2
	b .L344
.L343:
	bl inflate_fixed
.L344:
	lwz 0,36(1)
	mtlr 0
	lwz 26,8(1)
	lwz 27,12(1)
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe7:
	.size	 inflate_block,.Lfe7-inflate_block
	.align 2
	.globl inflate
	.type	 inflate,@function
inflate:
	stwu 1,-32(1)
	mflr 0
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	addis 9,0,outcnt@ha
	li 0,0
	stw 0,outcnt@l(9)
	addis 9,0,bk@ha
	stw 0,bk@l(9)
	addis 9,0,bb@ha
	stw 0,bb@l(9)
	mr 31,0
	addis 30,0,hufts@ha
	mr 29,0
.L346:
	stw 29,hufts@l(30)
	addi 3,1,8
	bl inflate_block
	mr. 3,3
	bc 4,2,.L356
	lwz 0,hufts@l(30)
	cmplw 1,0,31
	bc 4,5,.L348
	mr 31,0
.L348:
	lwz 0,8(1)
	cmpwi 1,0,0
	bc 12,6,.L346
	addis 9,0,bk@ha
	lwz 0,bk@l(9)
	cmplwi 1,0,7
	bc 4,5,.L353
	mr 10,9
	addis 11,0,inptr@ha
.L354:
	lwz 0,bk@l(10)
	addic 0,0,-8
	stw 0,bk@l(10)
	lwz 9,inptr@l(11)
	addi 9,9,-1
	stw 9,inptr@l(11)
	cmplwi 1,0,7
	bc 12,5,.L354
.L353:
	addis 9,0,outcnt@ha
	lwz 0,outcnt@l(9)
	bl flush_window
	li 3,0
.L356:
	lwz 0,36(1)
	mtlr 0
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe8:
	.size	 inflate,.Lfe8-inflate
	.comm	bb,4,4
	.comm	bk,4,4
	.comm	hufts,4,4
	.ident	"GCC: (GNU) 2.7-97r2"
