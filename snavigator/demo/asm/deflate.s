	.file	"deflate.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.globl window_size
	.section ".sdata","aw"
	.align 2
	.type	 window_size,@object
	.size	 window_size,4
window_size:
	.long 65536
	.section ".data"
	.align 1
	.type	 configuration_table,@object
	.size	 configuration_table,80
configuration_table:
	.short 0
	.short 0
	.short 0
	.short 0
	.short 4
	.short 4
	.short 8
	.short 4
	.short 4
	.short 5
	.short 16
	.short 8
	.short 4
	.short 6
	.short 32
	.short 32
	.short 4
	.short 4
	.short 16
	.short 16
	.short 8
	.short 16
	.short 32
	.short 32
	.short 8
	.short 16
	.short 128
	.short 128
	.short 8
	.short 32
	.short 128
	.short 256
	.short 32
	.short 128
	.short 258
	.short 1024
	.short 32
	.short 258
	.short 258
	.short 4096
	.section	".rodata"
	.align 2
.LC0:
	.string	"bad pack level"
	.section ".text"
	.align 2
	.globl lm_init
	.type	 lm_init,@function
lm_init:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	mr 31,3
	mr 30,4
	addi 0,31,-1
	cmplwi 1,0,8
	bc 4,5,.L2
	addis 3,0,.LC0@ha
	addi 3,3,.LC0@l
	bl error
.L2:
	addis 9,0,compr_level@ha
	stw 31,compr_level@l(9)
	addis 3,0,prev+65536@ha
	addi 3,3,prev+65536@l
	li 4,0
	lis 5,0x1
	bl memset
	addis 11,0,max_lazy_match@ha
	addis 9,0,configuration_table@ha
	addi 9,9,configuration_table@l
	slwi 8,31,3
	add 10,8,9
	lhz 0,2(10)
	stw 0,max_lazy_match@l(11)
	addis 11,0,good_match@ha
	lhzx 0,9,8
	stw 0,good_match@l(11)
	addis 9,0,nice_match@ha
	lhz 0,4(10)
	stw 0,nice_match@l(9)
	addis 9,0,max_chain_length@ha
	lhz 0,6(10)
	stw 0,max_chain_length@l(9)
	cmpwi 1,31,1
	bc 4,6,.L3
	lhz 0,0(30)
	ori 0,0,4
	b .L17
.L3:
	cmpwi 1,31,9
	bc 4,6,.L4
	lhz 0,0(30)
	ori 0,0,2
.L17:
	sth 0,0(30)
.L4:
	addis 9,0,strstart@ha
	li 31,0
	stw 31,strstart@l(9)
	addis 9,0,block_start@ha
	stw 31,block_start@l(9)
	addis 30,0,lookahead@ha
	addis 9,0,read_buf@ha
	lwz 0,read_buf@l(9)
	addis 3,0,window@ha
	addi 3,3,window@l
	lis 4,0x1
	mtlr 0
	blrl
	stw 3,lookahead@l(30)
	xor 0,3,31
	subfic 9,0,0
	adde 0,9,0
	subfic 3,3,-1
	subfic 9,3,0
	adde 3,9,3
	or. 9,0,3
	bc 12,2,.L6
	addis 9,0,eofile@ha
	li 0,1
	stw 0,eofile@l(9)
	stw 31,lookahead@l(30)
	b .L1
.L6:
	addis 9,0,eofile@ha
	li 0,0
	stw 0,eofile@l(9)
	addis 9,0,lookahead@ha
	lwz 0,lookahead@l(9)
	cmplwi 1,0,261
	bc 12,5,.L8
	mr 30,9
	addis 31,0,eofile@ha
.L9:
	bl fill_window
	lwz 0,lookahead@l(30)
	cmplwi 1,0,261
	bc 12,5,.L8
	lwz 0,eofile@l(31)
	cmpwi 1,0,0
	bc 12,6,.L9
.L8:
	addis 9,0,ins_h@ha
	li 0,0
	stw 0,ins_h@l(9)
	mr 11,0
	mr 10,9
	addis 9,0,window@ha
	addi 8,9,window@l
.L15:
	lwz 0,ins_h@l(10)
	lbzx 9,8,11
	rlwinm 0,0,5,17,26
	xor 0,0,9
	stw 0,ins_h@l(10)
	addi 11,11,1
	cmplwi 1,11,1
	bc 4,5,.L15
.L1:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe1:
	.size	 lm_init,.Lfe1-lm_init
	.align 2
	.globl longest_match
	.type	 longest_match,@function
longest_match:
	stwu 1,-24(1)
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	mr 8,3
	addis 9,0,max_chain_length@ha
	lwz 7,max_chain_length@l(9)
	addis 11,0,strstart@ha
	addis 9,0,window@ha
	addi 9,9,window@l
	lwz 0,strstart@l(11)
	add 10,0,9
	addis 9,0,prev_length@ha
	lwz 3,prev_length@l(9)
	cmplwi 1,0,32506
	li 12,0
	bc 4,5,.L19
	addic 12,0,-32506
.L19:
	addis 9,0,strstart@ha
	lwz 0,strstart@l(9)
	addis 9,0,window+258@ha
	addi 9,9,window+258@l
	add 6,0,9
	add 9,3,10
	lbz 4,-1(9)
	lbzx 5,10,3
	addis 9,0,prev_length@ha
	addis 11,0,good_match@ha
	lwz 9,prev_length@l(9)
	lwz 0,good_match@l(11)
	subfc 11,0,9
	subfe 11,11,11
	srwi 0,7,2
	and 9,7,11
	andc 0,0,11
	or 7,9,0
	addis 9,0,window@ha
	addi 31,9,window@l
	addis 28,0,match_start@ha
	addis 9,0,nice_match@ha
	lwz 30,nice_match@l(9)
	addis 9,0,prev@ha
	addi 29,9,prev@l
.L22:
	add 11,8,31
	lbzx 0,11,3
	cmpw 1,0,5
	bc 4,6,.L24
	add 9,3,11
	lbz 0,-1(9)
	cmpw 1,0,4
	bc 4,6,.L24
	lbzx 0,8,31
	lbz 9,0(10)
	cmpw 1,0,9
	bc 4,6,.L24
	lbzu 0,1(11)
	lbz 9,1(10)
	cmpw 1,0,9
	bc 4,6,.L24
	addi 10,10,2
	addi 11,11,1
.L29:
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	lbzu 0,1(10)
	lbzu 9,1(11)
	cmpw 1,0,9
	bc 4,6,.L28
	cmplw 1,10,6
	bc 12,4,.L29
.L28:
	subf 0,10,6
	subfic 0,0,258
	addi 10,6,-258
	cmpw 1,0,3
	bc 4,5,.L24
	stw 8,match_start@l(28)
	mr 3,0
	cmpw 1,3,30
	bc 4,4,.L23
	add 9,3,10
	lbz 4,-1(9)
	lbzx 5,10,3
.L24:
	rlwinm 0,8,1,16,30
	lhzx 8,29,0
	cmplw 1,8,12
	bc 4,5,.L23
	addic. 7,7,-1
	bc 4,2,.L22
.L23:
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe2:
	.size	 longest_match,.Lfe2-longest_match
	.globl memcpy
	.align 2
	.type	 fill_window,@function
fill_window:
	stwu 1,-40(1)
	mflr 0
	stw 29,28(1)
	stw 30,32(1)
	stw 31,36(1)
	stw 0,44(1)
	addis 9,0,window_size@ha
	addis 11,0,lookahead@ha
	lwz 9,window_size@l(9)
	lwz 0,lookahead@l(11)
	subf 31,0,9
	addis 9,0,strstart@ha
	lwz 0,strstart@l(9)
	subf 31,0,31
	cmpwi 1,31,-1
	bc 4,6,.L37
	li 31,-2
	b .L38
.L37:
	addis 30,0,strstart@ha
	lwz 0,strstart@l(30)
	li 29,0
	ori 29,29,65273
	cmplw 1,0,29
	bc 4,5,.L38
	addis 3,0,window@ha
	addis 4,0,window+32768@ha
	addi 3,3,window@l
	addi 4,4,window+32768@l
	li 5,0
	ori 5,5,32768
	crxor 6,6,6
	bl memcpy
	addis 9,0,match_start@ha
	lwz 0,match_start@l(9)
	addic 0,0,-32768
	stw 0,match_start@l(9)
	lwz 0,strstart@l(30)
	addic 0,0,-32768
	stw 0,strstart@l(30)
	addis 9,0,block_start@ha
	lwz 0,block_start@l(9)
	addic 0,0,-32768
	stw 0,block_start@l(9)
	li 8,0
	addis 9,0,prev+65536@ha
	addi 9,9,prev+65536@l
	mr 6,8
.L43:
	add 10,8,8
	lhzx 11,10,9
	add 7,10,9
	cmplwi 1,11,32767
	bc 4,5,.L44
	addi 0,11,-32768
	sthx 0,10,9
	b .L42
.L44:
	sth 6,0(7)
.L42:
	addi 8,8,1
	cmplwi 1,8,32767
	bc 4,5,.L43
	li 8,0
	addis 9,0,prev@ha
	addi 9,9,prev@l
	mr 7,8
.L50:
	add 0,8,8
	lhzx 11,9,0
	mr 10,0
	cmplwi 1,11,32767
	bc 4,5,.L51
	addi 0,11,-32768
	sthx 0,9,10
	b .L49
.L51:
	sthx 7,9,10
.L49:
	addi 8,8,1
	cmplwi 1,8,32767
	bc 4,5,.L50
	addis 0,31,0x1
	addic 31,0,-32768
.L38:
	addis 30,0,eofile@ha
	lwz 0,eofile@l(30)
	cmpwi 1,0,0
	bc 4,6,.L54
	addis 10,0,read_buf@ha
	addis 11,0,strstart@ha
	addis 9,0,lookahead@ha
	lwz 3,lookahead@l(9)
	addis 9,0,window@ha
	addi 9,9,window@l
	add 3,3,9
	lwz 0,strstart@l(11)
	lwz 9,read_buf@l(10)
	add 3,3,0
	mr 4,31
	mtlr 9
	blrl
	mr 8,3
	subfic 0,3,0
	adde 3,0,3
	subfic 0,8,-1
	subfic 9,0,0
	adde 0,9,0
	or. 9,3,0
	bc 12,2,.L55
	li 0,1
	stw 0,eofile@l(30)
	b .L54
.L55:
	addis 9,0,lookahead@ha
	lwz 0,lookahead@l(9)
	add 0,8,0
	stw 0,lookahead@l(9)
.L54:
	lwz 0,44(1)
	mtlr 0
	lwz 29,28(1)
	lwz 30,32(1)
	lwz 31,36(1)
	addi 1,1,40
	blr
.Lfe3:
	.size	 fill_window,.Lfe3-fill_window
	.align 2
	.type	 deflate_fast,@function
deflate_fast:
	stwu 1,-56(1)
	mflr 0
	stw 21,12(1)
	stw 22,16(1)
	stw 23,20(1)
	stw 24,24(1)
	stw 25,28(1)
	stw 26,32(1)
	stw 27,36(1)
	stw 28,40(1)
	stw 29,44(1)
	stw 30,48(1)
	stw 31,52(1)
	stw 0,60(1)
	li 31,0
	addis 9,0,prev_length@ha
	li 0,2
	stw 0,prev_length@l(9)
	addis 9,0,lookahead@ha
	lwz 0,lookahead@l(9)
	cmpw 1,0,31
	bc 12,6,.L59
	addis 26,0,ins_h@ha
	addis 9,0,window@ha
	addi 25,9,window@l
	addis 28,0,strstart@ha
	mr 9,28
	addi 21,9,strstart@l
	addis 9,0,prev@ha
	addi 22,9,prev@l
	addis 9,0,prev+65536@ha
	addi 24,9,prev+65536@l
	addis 27,0,lookahead@ha
	addis 23,0,block_start@ha
.L60:
	lwz 9,ins_h@l(26)
	lwz 11,strstart@l(28)
	addi 0,11,2
	lbzx 0,25,0
	rlwinm 9,9,5,17,26
	xor 9,9,0
	stw 9,ins_h@l(26)
	rlwinm 0,11,1,16,30
	add 9,9,9
	lhzx 3,9,24
	sthx 3,22,0
	lhz 0,2(21)
	sthx 0,9,24
	cmpwi 1,3,0
	bc 12,6,.L61
	subf 0,3,11
	cmplwi 1,0,32506
	bc 12,5,.L61
	bl longest_match
	mr 31,3
	lwz 0,lookahead@l(27)
	cmplw 1,31,0
	bc 4,5,.L61
	mr 31,0
.L61:
	cmplwi 1,31,2
	bc 4,5,.L63
	addis 9,0,match_start@ha
	lwz 0,strstart@l(28)
	lwz 3,match_start@l(9)
	subf 3,3,0
	addi 4,31,-3
	bl ct_tally
	mr 10,3
	lwz 0,lookahead@l(27)
	subf 0,31,0
	stw 0,lookahead@l(27)
	addis 9,0,max_lazy_match@ha
	lwz 0,max_lazy_match@l(9)
	cmplw 1,31,0
	bc 12,5,.L64
	addi 31,31,-1
.L65:
	lwz 9,strstart@l(28)
	addi 11,9,1
	stw 11,strstart@l(28)
	lwz 0,ins_h@l(26)
	addi 9,9,3
	lbzx 9,25,9
	rlwinm 0,0,5,17,26
	xor 0,0,9
	stw 0,ins_h@l(26)
	rlwinm 11,11,1,16,30
	add 0,0,0
	lhzx 3,24,0
	sthx 3,22,11
	lhz 9,2(21)
	sthx 9,24,0
	addic. 31,31,-1
	bc 4,2,.L65
	b .L82
.L64:
	lwz 0,strstart@l(28)
	add 0,31,0
	stw 0,strstart@l(28)
	li 31,0
	lbzx 9,25,0
	stw 9,ins_h@l(26)
	slwi 9,9,5
	addic 0,0,1
	lbzx 0,25,0
	xor 9,9,0
	stw 9,ins_h@l(26)
	b .L70
.L63:
	lwz 0,strstart@l(28)
	li 3,0
	lbzx 4,25,0
	bl ct_tally
	mr 10,3
	lwz 0,lookahead@l(27)
	addic 0,0,-1
	stw 0,lookahead@l(27)
.L82:
	lwz 0,strstart@l(28)
	addic 0,0,1
	stw 0,strstart@l(28)
.L70:
	cmpwi 1,10,0
	bc 12,6,.L71
	lwz 3,block_start@l(23)
	cmpwi 1,3,0
	bc 12,4,.L72
	add 3,3,25
	b .L73
.L72:
	li 3,0
.L73:
	lwz 0,strstart@l(28)
	lwz 4,block_start@l(23)
	subf 4,4,0
	li 5,0
	bl flush_block
	lwz 0,strstart@l(28)
	stw 0,block_start@l(23)
.L71:
	lwz 0,lookahead@l(27)
	cmplwi 1,0,261
	bc 12,5,.L58
	addis 9,0,eofile@ha
	lwz 0,eofile@l(9)
	cmpwi 1,0,0
	bc 4,6,.L58
	addis 29,0,lookahead@ha
	mr 30,9
.L76:
	bl fill_window
	lwz 0,lookahead@l(29)
	cmplwi 1,0,261
	bc 12,5,.L58
	lwz 0,eofile@l(30)
	cmpwi 1,0,0
	bc 12,6,.L76
.L58:
	lwz 0,lookahead@l(27)
	cmpwi 1,0,0
	bc 4,6,.L60
.L59:
	addis 9,0,block_start@ha
	lwz 3,block_start@l(9)
	cmpwi 1,3,0
	bc 12,4,.L80
	addis 9,0,window@ha
	addi 9,9,window@l
	add 3,3,9
	b .L81
.L80:
	li 3,0
.L81:
	addis 9,0,strstart@ha
	addis 11,0,block_start@ha
	lwz 0,strstart@l(9)
	lwz 4,block_start@l(11)
	subf 4,4,0
	li 5,1
	bl flush_block
	lwz 0,60(1)
	mtlr 0
	lwz 21,12(1)
	lwz 22,16(1)
	lwz 23,20(1)
	lwz 24,24(1)
	lwz 25,28(1)
	lwz 26,32(1)
	lwz 27,36(1)
	lwz 28,40(1)
	lwz 29,44(1)
	lwz 30,48(1)
	lwz 31,52(1)
	addi 1,1,56
	blr
.Lfe4:
	.size	 deflate_fast,.Lfe4-deflate_fast
	.align 2
	.globl deflate
	.type	 deflate,@function
deflate:
	stwu 1,-64(1)
	mflr 0
	stw 18,8(1)
	stw 19,12(1)
	stw 20,16(1)
	stw 21,20(1)
	stw 22,24(1)
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
	li 25,0
	li 29,2
	addis 9,0,compr_level@ha
	lwz 0,compr_level@l(9)
	cmpwi 1,0,3
	bc 12,5,.L84
	bl deflate_fast
	b .L114
.L84:
	addis 9,0,lookahead@ha
	lwz 0,lookahead@l(9)
	cmpwi 1,0,0
	bc 12,6,.L86
	addis 21,0,ins_h@ha
	addis 9,0,window@ha
	addi 23,9,window@l
	addis 28,0,strstart@ha
	mr 9,28
	addi 19,9,strstart@l
	addis 9,0,prev@ha
	addi 20,9,prev@l
	addis 9,0,prev+65536@ha
	addi 22,9,prev+65536@l
	addis 26,0,prev_length@ha
	addis 18,0,match_start@ha
	addis 27,0,lookahead@ha
	addis 24,0,block_start@ha
.L87:
	lwz 9,ins_h@l(21)
	lwz 10,strstart@l(28)
	addi 0,10,2
	lbzx 0,23,0
	rlwinm 9,9,5,17,26
	xor 9,9,0
	stw 9,ins_h@l(21)
	rlwinm 0,10,1,16,30
	add 9,9,9
	lhzx 3,9,22
	sthx 3,20,0
	lhz 0,2(19)
	sthx 0,9,22
	stw 29,prev_length@l(26)
	lwz 31,match_start@l(18)
	li 29,2
	cmpwi 1,3,0
	bc 12,6,.L88
	addis 9,0,max_lazy_match@ha
	lwz 11,prev_length@l(26)
	lwz 0,max_lazy_match@l(9)
	cmplw 1,11,0
	bc 4,4,.L88
	subf 0,3,10
	cmplwi 1,0,32506
	bc 12,5,.L88
	bl longest_match
	mr 29,3
	lwz 0,lookahead@l(27)
	cmplw 1,29,0
	bc 4,5,.L89
	mr 29,0
.L89:
	cmpwi 1,29,3
	bc 4,6,.L88
	lwz 0,strstart@l(28)
	lwz 9,match_start@l(18)
	subf 0,9,0
	subfic 0,0,4096
	subfe 0,0,0
	nand 0,0,0
	rlwinm 9,0,0,30,31
	nor 0,0,0
	rlwinm 0,0,0,30,30
	or 29,9,0
.L88:
	lwz 4,prev_length@l(26)
	cmplwi 1,4,2
	bc 4,5,.L91
	cmplw 1,29,4
	bc 12,5,.L91
	addi 0,31,1
	lwz 3,strstart@l(28)
	subf 3,0,3
	addi 4,4,-3
	bl ct_tally
	mr 10,3
	lwz 0,lookahead@l(27)
	addic 0,0,1
	lwz 9,prev_length@l(26)
	subf 0,9,0
	stw 0,lookahead@l(27)
	addi 9,9,-2
	stw 9,prev_length@l(26)
.L92:
	lwz 9,strstart@l(28)
	addi 11,9,1
	stw 11,strstart@l(28)
	lwz 0,ins_h@l(21)
	addi 9,9,3
	lbzx 9,23,9
	rlwinm 0,0,5,17,26
	xor 0,0,9
	stw 0,ins_h@l(21)
	rlwinm 11,11,1,16,30
	add 0,0,0
	lhzx 3,22,0
	sthx 3,20,11
	lhz 9,2(19)
	sthx 9,22,0
	lwz 0,prev_length@l(26)
	addic 0,0,-1
	stw 0,prev_length@l(26)
	cmpwi 1,0,0
	bc 4,6,.L92
	li 25,0
	li 29,2
	lwz 0,strstart@l(28)
	addic 0,0,1
	stw 0,strstart@l(28)
	cmpw 1,10,25
	bc 12,6,.L99
	lwz 3,block_start@l(24)
	cmpw 1,3,25
	bc 12,4,.L97
	add 3,3,23
	b .L98
.L97:
	li 3,0
.L98:
	lwz 0,strstart@l(28)
	lwz 4,block_start@l(24)
	subf 4,4,0
	li 5,0
	bl flush_block
	lwz 0,strstart@l(28)
	stw 0,block_start@l(24)
	b .L99
.L91:
	cmpwi 1,25,0
	bc 12,6,.L100
	lwz 0,strstart@l(28)
	addic 0,0,-1
	li 3,0
	lbzx 4,23,0
	bl ct_tally
	cmpwi 1,3,0
	bc 12,6,.L101
	lwz 3,block_start@l(24)
	cmpwi 1,3,0
	bc 12,4,.L102
	add 3,3,23
	b .L103
.L102:
	li 3,0
.L103:
	lwz 0,strstart@l(28)
	lwz 4,block_start@l(24)
	subf 4,4,0
	li 5,0
	bl flush_block
	lwz 0,strstart@l(28)
	stw 0,block_start@l(24)
.L101:
	lwz 0,strstart@l(28)
	addic 0,0,1
	b .L115
.L100:
	li 25,1
	lwz 0,strstart@l(28)
	add 0,0,25
.L115:
	stw 0,strstart@l(28)
	lwz 0,lookahead@l(27)
	addic 0,0,-1
	stw 0,lookahead@l(27)
.L99:
	lwz 0,lookahead@l(27)
	cmplwi 1,0,261
	bc 12,5,.L85
	addis 9,0,eofile@ha
	lwz 0,eofile@l(9)
	cmpwi 1,0,0
	bc 4,6,.L85
	addis 30,0,lookahead@ha
	mr 31,9
.L107:
	bl fill_window
	lwz 0,lookahead@l(30)
	cmplwi 1,0,261
	bc 12,5,.L85
	lwz 0,eofile@l(31)
	cmpwi 1,0,0
	bc 12,6,.L107
.L85:
	lwz 0,lookahead@l(27)
	cmpwi 1,0,0
	bc 4,6,.L87
.L86:
	cmpwi 1,25,0
	bc 12,6,.L111
	addis 11,0,window@ha
	addi 11,11,window@l
	addis 9,0,strstart@ha
	lwz 0,strstart@l(9)
	addic 0,0,-1
	li 3,0
	lbzx 4,11,0
	bl ct_tally
.L111:
	addis 9,0,block_start@ha
	lwz 3,block_start@l(9)
	cmpwi 1,3,0
	bc 12,4,.L112
	addis 9,0,window@ha
	addi 9,9,window@l
	add 3,3,9
	b .L113
.L112:
	li 3,0
.L113:
	addis 9,0,strstart@ha
	addis 11,0,block_start@ha
	lwz 0,strstart@l(9)
	lwz 4,block_start@l(11)
	subf 4,4,0
	li 5,1
	bl flush_block
.L114:
	lwz 0,68(1)
	mtlr 0
	lwz 18,8(1)
	lwz 19,12(1)
	lwz 20,16(1)
	lwz 21,20(1)
	lwz 22,24(1)
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
.Lfe5:
	.size	 deflate,.Lfe5-deflate
	.comm	block_start,4,4
	.section ".sdata","aw"
	.align 2
ins_h:
	.space	4
	.size	 ins_h,4
	.comm	prev_length,4,4
	.comm	strstart,4,4
	.comm	match_start,4,4
	.align 2
eofile:
	.space	4
	.size	 eofile,4
	.align 2
lookahead:
	.space	4
	.size	 lookahead,4
	.comm	max_chain_length,4,4
	.align 2
max_lazy_match:
	.space	4
	.size	 max_lazy_match,4
	.align 2
compr_level:
	.space	4
	.size	 compr_level,4
	.comm	good_match,4,4
	.comm	nice_match,4,4
	.ident	"GCC: (GNU) 2.7-97r2"
