	.file	"trees.c"

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
	.type	 extra_lbits,@object
	.size	 extra_lbits,116
extra_lbits:
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 1
	.long 1
	.long 1
	.long 1
	.long 2
	.long 2
	.long 2
	.long 2
	.long 3
	.long 3
	.long 3
	.long 3
	.long 4
	.long 4
	.long 4
	.long 4
	.long 5
	.long 5
	.long 5
	.long 5
	.long 0
	.align 2
	.type	 extra_dbits,@object
	.size	 extra_dbits,120
extra_dbits:
	.long 0
	.long 0
	.long 0
	.long 0
	.long 1
	.long 1
	.long 2
	.long 2
	.long 3
	.long 3
	.long 4
	.long 4
	.long 5
	.long 5
	.long 6
	.long 6
	.long 7
	.long 7
	.long 8
	.long 8
	.long 9
	.long 9
	.long 10
	.long 10
	.long 11
	.long 11
	.long 12
	.long 12
	.long 13
	.long 13
	.align 2
	.type	 extra_blbits,@object
	.size	 extra_blbits,76
extra_blbits:
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0
	.long 2
	.long 3
	.long 7
	.align 2
	.type	 l_desc,@object
	.size	 l_desc,28
l_desc:
	.long dyn_ltree
	.long static_ltree
	.long extra_lbits
	.long 257
	.long 286
	.long 15
	.long 0
	.align 2
	.type	 d_desc,@object
	.size	 d_desc,28
d_desc:
	.long dyn_dtree
	.long static_dtree
	.long extra_dbits
	.long 0
	.long 30
	.long 15
	.long 0
	.align 2
	.type	 bl_desc,@object
	.size	 bl_desc,28
bl_desc:
	.long bl_tree
	.long 0
	.long extra_blbits
	.long 0
	.long 19
	.long 7
	.long 0
	.align 2
	.type	 bl_order,@object
	.size	 bl_order,19
bl_order:
	.byte 16
	.byte 17
	.byte 18
	.byte 0
	.byte 8
	.byte 7
	.byte 9
	.byte 6
	.byte 10
	.byte 5
	.byte 11
	.byte 4
	.byte 12
	.byte 3
	.byte 13
	.byte 2
	.byte 14
	.byte 1
	.byte 15
	.section ".text"
	.align 2
	.globl ct_init
	.type	 ct_init,@function
ct_init:
	stwu 1,-24(1)
	mflr 0
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 9,0,file_type@ha
	stw 3,file_type@l(9)
	addis 9,0,file_method@ha
	stw 4,file_method@l(9)
	addis 11,0,compressed_len@ha
	addis 9,0,input_len@ha
	li 0,0
	stw 0,input_len@l(9)
	stw 0,compressed_len@l(11)
	addis 9,0,static_dtree+2@ha
	lhz 0,static_dtree+2@l(9)
	cmpwi 1,0,0
	bc 4,6,.L1
	li 8,0
	mr 10,8
	addis 9,0,base_length@ha
	addi 3,9,base_length@l
	addis 9,0,extra_lbits@ha
	addi 4,9,extra_lbits@l
	li 5,1
	addis 9,0,length_code@ha
	addi 6,9,length_code@l
.L6:
	slwi 0,10,2
	mr 7,0
	stwx 8,3,0
	lwzx 0,4,0
	li 31,0
	slw. 9,5,0
	bc 4,1,.L5
	addis 9,0,extra_lbits@ha
	addi 9,9,extra_lbits@l
	li 11,1
	lwzx 0,9,7
	slw 11,11,0
.L10:
	stbx 10,6,8
	addi 8,8,1
	addi 31,31,1
	cmpw 1,31,11
	bc 12,4,.L10
.L5:
	addi 10,10,1
	cmpwi 1,10,27
	bc 4,5,.L6
	addis 9,0,length_code@ha
	addi 9,9,length_code@l
	addi 0,8,-1
	stbx 10,9,0
	li 8,0
	mr 10,8
	addis 9,0,base_dist@ha
	addi 3,9,base_dist@l
	addis 9,0,extra_dbits@ha
	addi 4,9,extra_dbits@l
	li 5,1
	addis 9,0,dist_code@ha
	addi 6,9,dist_code@l
.L16:
	slwi 0,10,2
	mr 7,0
	stwx 8,3,0
	lwzx 0,4,0
	li 31,0
	slw. 9,5,0
	bc 4,1,.L15
	addis 9,0,extra_dbits@ha
	addi 9,9,extra_dbits@l
	li 11,1
	lwzx 0,9,7
	slw 11,11,0
.L20:
	stbx 10,6,8
	addi 8,8,1
	addi 31,31,1
	cmpw 1,31,11
	bc 12,4,.L20
.L15:
	addi 10,10,1
	cmpwi 1,10,15
	bc 4,5,.L16
	srawi 8,8,7
	cmpwi 1,10,29
	bc 12,5,.L24
	addis 9,0,base_dist@ha
	addi 4,9,base_dist@l
	addis 9,0,extra_dbits@ha
	addi 5,9,extra_dbits@l
	li 6,1
	addis 9,0,dist_code@ha
	addi 7,9,dist_code@l
.L26:
	slwi 9,10,2
	mr 11,9
	slwi 0,8,7
	stwx 0,4,9
	lwzx 0,5,9
	addic 0,0,-7
	li 31,0
	slw. 9,6,0
	bc 4,1,.L25
	addis 9,0,extra_dbits@ha
	addi 9,9,extra_dbits@l
	lwzx 0,9,11
	addic 0,0,-7
	li 9,1
	slw 9,9,0
.L30:
	addi 0,8,256
	stbx 10,7,0
	addi 8,8,1
	addi 31,31,1
	cmpw 1,31,9
	bc 12,4,.L30
.L25:
	addi 10,10,1
	cmpwi 1,10,29
	bc 4,5,.L26
.L24:
	li 11,0
	addis 9,0,bl_count@ha
	addi 9,9,bl_count@l
	mr 10,11
.L36:
	add 0,11,11
	sthx 10,9,0
	addi 11,11,1
	cmpwi 1,11,15
	bc 4,5,.L36
	li 31,0
	addis 9,0,static_ltree@ha
	addi 8,9,static_ltree@l
	li 10,8
	addis 9,0,bl_count@ha
	addi 11,9,bl_count@l
.L40:
	slwi 9,31,2
	add 9,9,8
	sth 10,2(9)
	addi 31,31,1
	lhz 0,16(11)
	addic 0,0,1
	sth 0,16(11)
	cmpwi 1,31,143
	bc 4,5,.L40
	cmpwi 1,31,255
	bc 12,5,.L59
	addis 9,0,static_ltree@ha
	addi 8,9,static_ltree@l
	li 10,9
	addis 9,0,bl_count@ha
	addi 11,9,bl_count@l
.L44:
	slwi 9,31,2
	add 9,9,8
	sth 10,2(9)
	addi 31,31,1
	lhz 0,18(11)
	addic 0,0,1
	sth 0,18(11)
	cmpwi 1,31,255
	bc 4,5,.L44
.L59:
	cmpwi 1,31,279
	bc 12,5,.L60
	addis 9,0,static_ltree@ha
	addi 8,9,static_ltree@l
	li 10,7
	addis 9,0,bl_count@ha
	addi 11,9,bl_count@l
.L48:
	slwi 9,31,2
	add 9,9,8
	sth 10,2(9)
	addi 31,31,1
	lhz 0,14(11)
	addic 0,0,1
	sth 0,14(11)
	cmpwi 1,31,279
	bc 4,5,.L48
.L60:
	cmpwi 1,31,287
	bc 12,5,.L61
	addis 9,0,static_ltree@ha
	addi 8,9,static_ltree@l
	li 10,8
	addis 9,0,bl_count@ha
	addi 11,9,bl_count@l
.L52:
	slwi 9,31,2
	add 9,9,8
	sth 10,2(9)
	addi 31,31,1
	lhz 0,16(11)
	addic 0,0,1
	sth 0,16(11)
	cmpwi 1,31,287
	bc 4,5,.L52
.L61:
	addis 3,0,static_ltree@ha
	addi 3,3,static_ltree@l
	li 4,287
	bl gen_codes
	li 31,0
	addis 9,0,static_dtree@ha
	addi 30,9,static_dtree@l
	li 28,5
.L57:
	slwi 29,31,2
	add 9,29,30
	sth 28,2(9)
	mr 3,31
	li 4,5
	bl bi_reverse
	sthx 3,30,29
	addi 31,31,1
	cmpwi 1,31,29
	bc 4,5,.L57
	bl init_block
.L1:
	lwz 0,28(1)
	mtlr 0
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe1:
	.size	 ct_init,.Lfe1-ct_init
	.align 2
	.type	 init_block,@function
init_block:
	li 11,0
	addis 9,0,dyn_ltree@ha
	addi 9,9,dyn_ltree@l
	mr 10,11
.L66:
	slwi 0,11,2
	sthx 10,9,0
	addi 11,11,1
	cmpwi 1,11,285
	bc 4,5,.L66
	li 11,0
	addis 9,0,dyn_dtree@ha
	addi 9,9,dyn_dtree@l
	mr 10,11
.L71:
	slwi 0,11,2
	sthx 10,9,0
	addi 11,11,1
	cmpwi 1,11,29
	bc 4,5,.L71
	li 11,0
	addis 9,0,bl_tree@ha
	addi 9,9,bl_tree@l
	mr 10,11
.L76:
	slwi 0,11,2
	sthx 10,9,0
	addi 11,11,1
	cmpwi 1,11,18
	bc 4,5,.L76
	addis 9,0,dyn_ltree+1024@ha
	li 8,1
	sth 8,dyn_ltree+1024@l(9)
	addis 11,0,opt_len@ha
	addis 9,0,static_len@ha
	li 0,0
	stw 0,static_len@l(9)
	stw 0,opt_len@l(11)
	addis 10,0,last_lit@ha
	addis 11,0,last_dist@ha
	addis 9,0,last_flags@ha
	stw 0,last_flags@l(9)
	stw 0,last_dist@l(11)
	stw 0,last_lit@l(10)
	addis 9,0,flags@ha
	stb 0,flags@l(9)
	addis 9,0,flag_bit@ha
	stb 8,flag_bit@l(9)
	blr
.Lfe2:
	.size	 init_block,.Lfe2-init_block
	.align 2
	.type	 pqdownheap,@function
pqdownheap:
	stwu 1,-16(1)
	stw 31,12(1)
	addis 9,0,heap@ha
	addi 9,9,heap@l
	slwi 0,4,2
	lwzx 12,9,0
	slwi 11,4,1
	addis 9,0,heap_len@ha
	lwz 0,heap_len@l(9)
	cmpw 1,11,0
	bc 12,5,.L80
	mr 5,0
	addis 9,0,heap@ha
	addi 7,9,heap@l
	addis 9,0,depth@ha
	addi 6,9,depth@l
	slwi 31,12,2
.L81:
	cmpw 1,11,5
	bc 4,4,.L82
	addi 0,11,1
	slwi 0,0,2
	lwzx 8,7,0
	slwi 9,8,2
	slwi 0,11,2
	lwzx 10,7,0
	slwi 0,10,2
	lhzx 9,9,3
	lhzx 0,3,0
	cmplw 1,9,0
	bc 12,4,.L83
	cmpw 1,9,0
	bc 4,6,.L82
	lbzx 0,6,8
	lbzx 9,6,10
	cmplw 1,0,9
	bc 12,5,.L82
.L83:
	addi 11,11,1
.L82:
	slwi 0,11,2
	lwzx 10,7,0
	slwi 0,10,2
	lhzx 9,31,3
	lhzx 0,3,0
	cmplw 1,9,0
	bc 12,4,.L80
	cmpw 1,9,0
	bc 4,6,.L84
	lbzx 0,6,12
	lbzx 9,6,10
	cmplw 1,0,9
	bc 4,5,.L80
.L84:
	slwi 9,4,2
	slwi 0,11,2
	lwzx 0,7,0
	stwx 0,7,9
	mr 4,11
	slwi 11,4,1
	cmpw 1,11,5
	bc 4,5,.L81
.L80:
	addis 9,0,heap@ha
	addi 9,9,heap@l
	slwi 0,4,2
	stwx 12,9,0
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe3:
	.size	 pqdownheap,.Lfe3-pqdownheap
	.align 2
	.type	 gen_bitlen,@function
gen_bitlen:
	stwu 1,-40(1)
	stw 25,12(1)
	stw 26,16(1)
	stw 27,20(1)
	stw 28,24(1)
	stw 29,28(1)
	stw 30,32(1)
	stw 31,36(1)
	lwz 5,0(3)
	lwz 26,8(3)
	lwz 31,12(3)
	lwz 27,24(3)
	lwz 12,20(3)
	lwz 3,4(3)
	li 4,0
	mr 11,4
	addis 9,0,bl_count@ha
	addi 9,9,bl_count@l
	mr 10,4
.L91:
	add 0,11,11
	sthx 10,9,0
	addi 11,11,1
	cmpwi 1,11,15
	bc 4,5,.L91
	addis 11,0,heap@ha
	addi 11,11,heap@l
	addis 9,0,heap_max@ha
	lwz 10,heap_max@l(9)
	slwi 0,10,2
	lwzx 9,11,0
	slwi 9,9,2
	add 9,9,5
	li 0,0
	sth 0,2(9)
	addi 10,10,1
	cmpwi 1,10,572
	bc 12,5,.L94
	addis 9,0,heap@ha
	addi 25,9,heap@l
	addis 9,0,bl_count@ha
	addi 28,9,bl_count@l
	addis 29,0,opt_len@ha
	cmpwi 6,3,0
	addis 30,0,static_len@ha
.L96:
	slwi 0,10,2
	lwzx 7,25,0
	slwi 9,7,2
	add 9,9,5
	lhz 9,2(9)
	slwi 9,9,2
	add 9,9,5
	lhz 0,2(9)
	addic 11,0,1
	cmpw 1,11,12
	bc 4,5,.L97
	mr 11,12
	addi 4,4,1
.L97:
	slwi 9,7,2
	add 9,9,5
	sth 11,2(9)
	cmpw 1,7,27
	bc 12,5,.L95
	add 0,11,11
	lhzx 9,28,0
	addi 9,9,1
	sthx 9,28,0
	li 8,0
	cmpw 1,7,31
	bc 12,4,.L99
	subf 0,31,7
	slwi 0,0,2
	lwzx 8,26,0
.L99:
	slwi 7,7,2
	lhzx 6,7,5
	add 0,11,8
	mullw 0,6,0
	lwz 9,opt_len@l(29)
	add 0,0,9
	stw 0,opt_len@l(29)
	bc 12,26,.L95
	add 9,7,3
	lhz 0,2(9)
	add 0,0,8
	mullw 0,6,0
	lwz 9,static_len@l(30)
	add 0,0,9
	stw 0,static_len@l(30)
.L95:
	addi 10,10,1
	cmpwi 1,10,572
	bc 4,5,.L96
.L94:
	cmpwi 1,4,0
	bc 12,6,.L87
	addis 9,0,bl_count@ha
	addi 8,9,bl_count@l
	add 7,12,12
.L103:
	addi 11,12,-1
	add 0,11,11
	lhzx 0,8,0
	cmpwi 1,0,0
	bc 4,6,.L107
	addis 9,0,bl_count@ha
	addi 9,9,bl_count@l
.L108:
	addi 11,11,-1
	add 0,11,11
	lhzx 0,9,0
	cmpwi 1,0,0
	bc 12,6,.L108
.L107:
	add 9,11,11
	lhzx 0,8,9
	addic 0,0,-1
	sthx 0,8,9
	addi 9,11,1
	add 9,9,9
	lhzx 0,8,9
	addic 0,0,2
	sthx 0,8,9
	lhzx 0,8,7
	addic 0,0,-1
	sthx 0,8,7
	addic. 4,4,-2
	bc 12,1,.L103
	mr. 11,12
	bc 12,2,.L87
	addis 9,0,bl_count@ha
	addi 3,9,bl_count@l
	addis 9,0,heap@ha
	addi 4,9,heap@l
	addis 6,0,opt_len@ha
.L114:
	add 0,11,11
	lhzx 7,3,0
.L122:
	cmpwi 1,7,0
	bc 12,6,.L113
	addi 10,10,-1
	slwi 0,10,2
	lwzx 0,4,0
	cmpw 1,0,27
	bc 12,5,.L122
	slwi 9,0,2
	add 8,9,5
	lhz 0,2(8)
	cmpw 1,0,11
	bc 12,6,.L119
	subf 0,0,11
	lhzx 9,9,5
	mullw 0,0,9
	lwz 9,opt_len@l(6)
	add 0,0,9
	stw 0,opt_len@l(6)
	sth 11,2(8)
.L119:
	addi 7,7,-1
	b .L122
.L113:
	addic. 11,11,-1
	bc 4,2,.L114
.L87:
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
	.size	 gen_bitlen,.Lfe4-gen_bitlen
	.align 2
	.type	 gen_codes,@function
gen_codes:
	stwu 1,-64(1)
	mflr 0
	stw 27,44(1)
	stw 28,48(1)
	stw 29,52(1)
	stw 30,56(1)
	stw 31,60(1)
	stw 0,68(1)
	mr 28,3
	mr 27,4
	li 10,0
	li 11,1
	addis 9,0,bl_count@ha
	addi 7,9,bl_count@l
	addi 8,1,8
.L127:
	add 9,11,11
	addi 0,11,-1
	add 0,0,0
	lhzx 0,7,0
	add 0,10,0
	slwi 0,0,1
	rlwinm 10,0,0,0xffff
	sthx 0,8,9
	addi 11,11,1
	cmpwi 1,11,15
	bc 4,5,.L127
	li 31,0
	cmpw 1,31,27
	bc 12,5,.L130
	addi 29,1,8
.L132:
	slwi 30,31,2
	add 9,30,28
	lhz 4,2(9)
	cmpwi 1,4,0
	bc 12,6,.L131
	add 9,4,4
	lhzx 3,29,9
	addi 0,3,1
	sthx 0,29,9
	rlwinm 3,3,0,0xffff
	bl bi_reverse
	sthx 3,30,28
.L131:
	addi 31,31,1
	cmpw 1,31,27
	bc 4,5,.L132
.L130:
	lwz 0,68(1)
	mtlr 0
	lwz 27,44(1)
	lwz 28,48(1)
	lwz 29,52(1)
	lwz 30,56(1)
	lwz 31,60(1)
	addi 1,1,64
	blr
.Lfe5:
	.size	 gen_codes,.Lfe5-gen_codes
	.align 2
	.type	 build_tree,@function
build_tree:
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
	mr 23,3
	lwz 30,0(23)
	lwz 5,4(23)
	lwz 11,16(23)
	li 28,-1
	mr 29,11
	addis 9,0,heap_len@ha
	li 0,0
	stw 0,heap_len@l(9)
	addis 9,0,heap_max@ha
	li 0,573
	stw 0,heap_max@l(9)
	li 31,0
	cmpw 1,31,29
	bc 4,4,.L137
	addis 9,0,heap@ha
	addi 4,9,heap@l
	addis 10,0,heap_len@ha
	addis 9,0,depth@ha
	addi 6,9,depth@l
	mr 7,31
	mr 8,31
.L139:
	slwi 0,31,2
	lhzx 0,30,0
	cmpwi 1,0,0
	bc 12,6,.L140
	lwz 0,heap_len@l(10)
	addic 0,0,1
	stw 0,heap_len@l(10)
	slwi 0,0,2
	mr 28,31
	stwx 28,4,0
	stbx 7,6,28
	b .L138
.L140:
	slwi 9,31,2
	add 9,9,30
	sth 8,2(9)
.L138:
	addi 31,31,1
	cmpw 1,31,11
	bc 12,4,.L139
.L137:
	addis 9,0,heap_len@ha
	lwz 0,heap_len@l(9)
	cmpwi 1,0,1
	bc 12,5,.L161
	addis 9,0,heap@ha
	addi 6,9,heap@l
	addis 11,0,heap_len@ha
	li 7,0
	li 3,1
	addis 9,0,depth@ha
	addi 4,9,depth@l
	addis 8,0,opt_len@ha
	cmpw 6,5,7
	addis 10,0,static_len@ha
.L145:
	lwz 0,heap_len@l(11)
	addic 0,0,1
	stw 0,heap_len@l(11)
	slwi 0,0,2
	add 9,0,6
	cmpwi 1,28,1
	bc 12,5,.L146
	addi 28,28,1
	stwx 28,6,0
	b .L147
.L146:
	stw 7,0(9)
.L147:
	lwz 0,0(9)
	slwi 9,0,2
	sthx 3,9,30
	stbx 7,4,0
	lwz 0,opt_len@l(8)
	addic 0,0,-1
	stw 0,opt_len@l(8)
	bc 12,26,.L143
	add 9,9,5
	lhz 9,2(9)
	lwz 0,static_len@l(10)
	subf 0,9,0
	stw 0,static_len@l(10)
.L143:
	lwz 0,heap_len@l(11)
	cmpwi 1,0,1
	bc 4,5,.L145
.L161:
	stw 28,24(23)
	addis 9,0,heap_len@ha
	lwz 0,heap_len@l(9)
	srwi 9,0,31
	add 0,0,9
	srawi. 31,0,1
	bc 4,1,.L151
.L153:
	mr 3,30
	mr 4,31
	bl pqdownheap
	addic. 31,31,-1
	bc 12,1,.L153
.L151:
	addis 9,0,heap@ha
	addi 27,9,heap@l
	addis 24,0,heap_len@ha
	addis 25,0,heap_max@ha
	addis 9,0,depth@ha
	addi 26,9,depth@l
.L155:
	lwz 31,4(27)
	lwz 9,heap_len@l(24)
	slwi 0,9,2
	lwzx 0,27,0
	stw 0,4(27)
	addi 9,9,-1
	stw 9,heap_len@l(24)
	mr 3,30
	li 4,1
	bl pqdownheap
	lwz 10,4(27)
	lwz 0,heap_max@l(25)
	addic 0,0,-1
	stw 0,heap_max@l(25)
	slwi 9,0,2
	stwx 31,27,9
	addic 0,0,-1
	stw 0,heap_max@l(25)
	slwi 0,0,2
	stwx 10,27,0
	slwi 11,29,2
	slwi 0,31,2
	slwi 9,10,2
	lhzx 0,30,0
	lhzx 9,9,30
	add 0,0,9
	sthx 0,11,30
	lbzx 11,26,31
	rlwinm 9,11,0,0xff
	lbzx 0,26,10
	cmplw 1,9,0
	bc 12,4,.L158
	addi 0,11,1
	b .L159
.L158:
	lbzx 0,26,10
	addic 0,0,1
.L159:
	stbx 0,26,29
	slwi 11,31,2
	add 11,11,30
	slwi 9,10,2
	add 9,9,30
	sth 29,2(9)
	sth 29,2(11)
	stw 29,4(27)
	addi 29,29,1
	mr 3,30
	li 4,1
	bl pqdownheap
	lwz 0,heap_len@l(24)
	cmpwi 1,0,1
	bc 12,5,.L155
	addis 11,0,heap@ha
	addi 11,11,heap@l
	addis 9,0,heap_max@ha
	lwz 0,heap_max@l(9)
	addic 0,0,-1
	stw 0,heap_max@l(9)
	slwi 0,0,2
	lwz 9,4(11)
	stwx 9,11,0
	mr 3,23
	bl gen_bitlen
	mr 3,30
	mr 4,28
	bl gen_codes
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
.Lfe6:
	.size	 build_tree,.Lfe6-build_tree
	.align 2
	.type	 scan_tree,@function
scan_tree:
	stwu 1,-16(1)
	stw 31,12(1)
	mr 31,3
	mr 3,4
	li 4,-1
	lhz 5,2(31)
	li 8,0
	li 7,7
	li 10,4
	cmpw 1,5,8
	bc 4,6,.L163
	li 7,138
	li 10,3
.L163:
	slwi 9,3,2
	add 9,9,31
	li 0,-1
	sth 0,6(9)
	li 12,0
	cmpw 1,12,3
	bc 12,5,.L165
	addis 9,0,bl_tree@ha
	addi 6,9,bl_tree@l
.L167:
	mr 11,5
	slwi 9,12,2
	add 9,9,31
	lhz 5,6(9)
	addi 8,8,1
	cmpw 1,8,7
	bc 4,4,.L168
	cmpw 1,11,5
	bc 12,6,.L166
.L168:
	cmpw 1,8,10
	bc 4,4,.L170
	slwi 9,11,2
	lhzx 0,6,9
	add 0,0,8
	sthx 0,6,9
	b .L169
.L170:
	cmpwi 1,11,0
	bc 12,6,.L172
	cmpw 1,11,4
	bc 12,6,.L173
	slwi 9,11,2
	lhzx 0,6,9
	addic 0,0,1
	sthx 0,6,9
.L173:
	lhz 0,64(6)
	addic 0,0,1
	sth 0,64(6)
	b .L169
.L172:
	cmpwi 1,8,10
	bc 12,5,.L175
	lhz 0,68(6)
	addic 0,0,1
	sth 0,68(6)
	b .L169
.L175:
	lhz 0,72(6)
	addic 0,0,1
	sth 0,72(6)
.L169:
	li 8,0
	mr 4,11
	cmpw 1,5,8
	bc 4,6,.L177
	li 7,138
	li 10,3
	b .L166
.L177:
	cmpw 1,11,5
	bc 4,6,.L179
	li 7,6
	li 10,3
	b .L166
.L179:
	li 7,7
	li 10,4
.L166:
	addi 12,12,1
	cmpw 1,12,3
	bc 4,5,.L167
.L165:
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe7:
	.size	 scan_tree,.Lfe7-scan_tree
	.align 2
	.type	 send_tree,@function
send_tree:
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
	mr 23,3
	mr 24,4
	li 10,-1
	lhz 26,2(23)
	li 31,0
	li 11,7
	li 0,4
	cmpw 1,26,31
	bc 4,6,.L183
	li 11,138
	li 0,3
.L183:
	li 25,0
	cmpw 1,25,24
	bc 12,5,.L185
	addis 9,0,bl_tree@ha
	addi 27,9,bl_tree@l
.L187:
	mr 28,26
	slwi 9,25,2
	add 9,9,23
	lhz 26,6(9)
	addi 31,31,1
	cmpw 1,31,11
	bc 4,4,.L188
	cmpw 1,28,26
	bc 12,6,.L186
.L188:
	cmpw 1,31,0
	bc 4,4,.L190
	slwi 30,28,2
	add 29,30,27
.L191:
	lhzx 3,27,30
	lhz 4,2(29)
	bl send_bits
	addic. 31,31,-1
	bc 4,2,.L191
	b .L189
.L190:
	cmpwi 1,28,0
	bc 12,6,.L196
	cmpw 1,28,10
	bc 12,6,.L197
	slwi 0,28,2
	add 9,0,27
	lhzx 3,27,0
	lhz 4,2(9)
	bl send_bits
	addi 31,31,-1
.L197:
	lhz 3,64(27)
	lhz 4,66(27)
	bl send_bits
	addi 3,31,-3
	li 4,2
	b .L206
.L196:
	cmpwi 1,31,10
	bc 12,5,.L199
	lhz 3,68(27)
	lhz 4,70(27)
	bl send_bits
	addi 3,31,-3
	li 4,3
	b .L206
.L199:
	lhz 3,72(27)
	lhz 4,74(27)
	bl send_bits
	addi 3,31,-11
	li 4,7
.L206:
	bl send_bits
.L189:
	li 31,0
	mr 10,28
	cmpw 1,26,31
	bc 4,6,.L201
	li 11,138
	li 0,3
	b .L186
.L201:
	cmpw 1,28,26
	bc 4,6,.L203
	li 11,6
	li 0,3
	b .L186
.L203:
	li 11,7
	li 0,4
.L186:
	addi 25,25,1
	cmpw 1,25,24
	bc 4,5,.L187
.L185:
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
.Lfe8:
	.size	 send_tree,.Lfe8-send_tree
	.align 2
	.type	 build_bl_tree,@function
build_bl_tree:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,l_desc+24@ha
	addis 3,0,dyn_ltree@ha
	addi 3,3,dyn_ltree@l
	lwz 4,l_desc+24@l(9)
	bl scan_tree
	addis 9,0,d_desc+24@ha
	addis 3,0,dyn_dtree@ha
	addi 3,3,dyn_dtree@l
	lwz 4,d_desc+24@l(9)
	bl scan_tree
	addis 3,0,bl_desc@ha
	addi 3,3,bl_desc@l
	bl build_tree
	li 3,18
	addis 9,0,bl_tree@ha
	addi 10,9,bl_tree@l
	addis 9,0,bl_order@ha
	addi 11,9,bl_order@l
.L211:
	lbzx 9,11,3
	slwi 9,9,2
	add 9,9,10
	lhz 0,2(9)
	cmpwi 1,0,0
	bc 4,6,.L209
	addi 3,3,-1
	cmpwi 1,3,2
	bc 12,5,.L211
.L209:
	addis 10,0,opt_len@ha
	lwz 9,opt_len@l(10)
	addi 9,9,14
	addi 11,3,1
	slwi 0,11,1
	add 0,0,11
	add 9,9,0
	stw 9,opt_len@l(10)
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe9:
	.size	 build_bl_tree,.Lfe9-build_bl_tree
	.align 2
	.type	 send_all_trees,@function
send_all_trees:
	stwu 1,-32(1)
	mflr 0
	stw 26,8(1)
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 27,3
	mr 26,4
	mr 30,5
	addi 3,27,-257
	li 4,5
	bl send_bits
	addi 3,26,-1
	li 4,5
	bl send_bits
	addi 3,30,-4
	li 4,4
	bl send_bits
	li 31,0
	cmpw 1,31,30
	bc 4,4,.L216
	addis 9,0,bl_tree@ha
	addi 28,9,bl_tree@l
	addis 9,0,bl_order@ha
	addi 29,9,bl_order@l
.L218:
	lbzx 9,29,31
	slwi 9,9,2
	add 9,9,28
	lhz 3,2(9)
	li 4,3
	bl send_bits
	addi 31,31,1
	cmpw 1,31,30
	bc 12,4,.L218
.L216:
	addis 3,0,dyn_ltree@ha
	addi 3,3,dyn_ltree@l
	addi 4,27,-1
	bl send_tree
	addis 3,0,dyn_dtree@ha
	addi 3,3,dyn_dtree@l
	addi 4,26,-1
	bl send_tree
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
.Lfe10:
	.size	 send_all_trees,.Lfe10-send_all_trees
	.section	".rodata"
	.align 2
.LC0:
	.string	"block vanished"
	.section ".text"
	.align 2
	.globl flush_block
	.type	 flush_block,@function
flush_block:
	stwu 1,-32(1)
	mflr 0
	stw 27,12(1)
	stw 28,16(1)
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	mr 28,3
	mr 30,4
	mr 29,5
	addis 11,0,flag_buf@ha
	addi 11,11,flag_buf@l
	addis 9,0,last_flags@ha
	lwz 10,last_flags@l(9)
	addis 9,0,flags@ha
	lbz 0,flags@l(9)
	stbx 0,11,10
	addis 9,0,file_type@ha
	lwz 9,file_type@l(9)
	lhz 9,0(9)
	li 0,0
	ori 0,0,65535
	cmpw 1,9,0
	bc 4,6,.L221
	bl set_file_type
.L221:
	addis 3,0,l_desc@ha
	addi 3,3,l_desc@l
	bl build_tree
	addis 3,0,d_desc@ha
	addi 3,3,d_desc@l
	bl build_tree
	bl build_bl_tree
	mr 27,3
	addis 9,0,opt_len@ha
	lwz 0,opt_len@l(9)
	addic 10,0,10
	srwi 10,10,3
	addis 9,0,static_len@ha
	lwz 0,static_len@l(9)
	addic 8,0,10
	srwi 8,8,3
	addis 9,0,input_len@ha
	lwz 0,input_len@l(9)
	add 0,30,0
	stw 0,input_len@l(9)
	subfc 9,8,10
	subfe 9,9,9
	and 0,10,9
	andc 9,8,9
	or 10,0,9
	addi 31,30,4
	subfc 0,31,10
	li 0,0
	adde 0,0,0
	addic 9,28,-1
	subfe 11,9,28
	and. 9,0,11
	bc 12,2,.L226
	mr 3,29
	li 4,3
	bl send_bits
	addis 11,0,compressed_len@ha
	lwz 9,compressed_len@l(11)
	addi 9,9,10
	rlwinm 9,9,0,0,28
	slwi 0,31,3
	add 0,0,9
	stw 0,compressed_len@l(11)
	mr 3,28
	mr 4,30
	li 5,1
	bl copy_block
	b .L225
.L226:
	cmpw 1,8,10
	bc 4,6,.L228
	addi 3,29,2
	li 4,3
	bl send_bits
	addis 3,0,static_ltree@ha
	addi 3,3,static_ltree@l
	addis 4,0,static_dtree@ha
	addi 4,4,static_dtree@l
	bl compress_block
	addis 11,0,compressed_len@ha
	lwz 0,compressed_len@l(11)
	addic 0,0,3
	addis 9,0,static_len@ha
	lwz 9,static_len@l(9)
	b .L231
.L228:
	addi 3,29,4
	li 4,3
	bl send_bits
	addis 9,0,l_desc+24@ha
	lwz 3,l_desc+24@l(9)
	addis 9,0,d_desc+24@ha
	lwz 4,d_desc+24@l(9)
	addi 3,3,1
	addi 4,4,1
	addi 5,27,1
	bl send_all_trees
	addis 3,0,dyn_ltree@ha
	addi 3,3,dyn_ltree@l
	addis 4,0,dyn_dtree@ha
	addi 4,4,dyn_dtree@l
	bl compress_block
	addis 11,0,compressed_len@ha
	lwz 0,compressed_len@l(11)
	addic 0,0,3
	addis 9,0,opt_len@ha
	lwz 9,opt_len@l(9)
.L231:
	add 0,0,9
	stw 0,compressed_len@l(11)
.L225:
	bl init_block
	cmpwi 1,29,0
	bc 12,6,.L230
	bl bi_windup
	addis 9,0,compressed_len@ha
	lwz 0,compressed_len@l(9)
	addic 0,0,7
	stw 0,compressed_len@l(9)
.L230:
	addis 9,0,compressed_len@ha
	lwz 3,compressed_len@l(9)
	srwi 3,3,3
	lwz 0,36(1)
	mtlr 0
	lwz 27,12(1)
	lwz 28,16(1)
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	addi 1,1,32
	blr
.Lfe11:
	.size	 flush_block,.Lfe11-flush_block
	.align 2
	.globl ct_tally
	.type	 ct_tally,@function
ct_tally:
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	addis 11,0,last_lit@ha
	lwz 0,last_lit@l(11)
	stbx 4,9,0
	addic 0,0,1
	stw 0,last_lit@l(11)
	mr. 3,3
	bc 4,2,.L233
	addis 9,0,dyn_ltree@ha
	addi 9,9,dyn_ltree@l
	slwi 11,4,2
	lhzx 0,9,11
	addic 0,0,1
	sthx 0,9,11
	b .L234
.L233:
	addi 3,3,-1
	addis 11,0,dyn_ltree@ha
	addis 9,0,length_code@ha
	addi 9,9,length_code@l
	lbzx 9,9,4
	addi 9,9,257
	addi 11,11,dyn_ltree@l
	slwi 9,9,2
	lhzx 0,11,9
	addic 0,0,1
	sthx 0,11,9
	cmpwi 1,3,255
	addis 9,0,dyn_dtree@ha
	addi 8,9,dyn_dtree@l
	cmpwi 1,3,255
	bc 12,5,.L237
	addis 9,0,dist_code@ha
	addi 9,9,dist_code@l
	lbzx 11,9,3
	b .L252
.L237:
	addis 9,0,dist_code@ha
	addi 9,9,dist_code@l
	srawi 0,3,7
	addic 0,0,256
	lbzx 11,9,0
.L252:
	slwi 11,11,2
	addis 9,0,dyn_dtree@ha
	addi 10,9,dyn_dtree@l
	cmpwi 1,3,255
	bc 12,5,.L239
	addis 9,0,dist_code@ha
	addi 9,9,dist_code@l
	lbzx 0,9,3
	b .L253
.L239:
	addis 9,0,dist_code@ha
	addi 9,9,dist_code@l
	srawi 0,3,7
	addic 0,0,256
	lbzx 0,9,0
.L253:
	slwi 0,0,2
	lhzx 0,10,0
	addic 0,0,1
	sthx 0,8,11
	addis 11,0,d_buf@ha
	addi 11,11,d_buf@l
	addis 10,0,last_dist@ha
	lwz 0,last_dist@l(10)
	add 9,0,0
	sthx 3,11,9
	addic 0,0,1
	stw 0,last_dist@l(10)
	addis 11,0,flags@ha
	addis 9,0,flag_bit@ha
	lbz 0,flags@l(11)
	lbz 9,flag_bit@l(9)
	or 0,0,9
	stb 0,flags@l(11)
.L234:
	addis 6,0,flag_bit@ha
	lbz 0,flag_bit@l(6)
	slwi 0,0,1
	stb 0,flag_bit@l(6)
	addis 9,0,last_lit@ha
	lwz 0,last_lit@l(9)
	andi. 7,0,7
	bc 4,2,.L241
	addis 11,0,flag_buf@ha
	addi 11,11,flag_buf@l
	addis 8,0,last_flags@ha
	lwz 0,last_flags@l(8)
	addis 10,0,flags@ha
	lbz 9,flags@l(10)
	stbx 9,11,0
	addic 0,0,1
	stw 0,last_flags@l(8)
	stb 7,flags@l(10)
	li 0,1
	stb 0,flag_bit@l(6)
.L241:
	addis 9,0,level@ha
	lwz 0,level@l(9)
	cmpwi 1,0,2
	bc 4,5,.L242
	addis 9,0,last_lit@ha
	lwz 9,last_lit@l(9)
	andi. 0,9,4095
	bc 4,2,.L242
	slwi 10,9,3
	addis 9,0,strstart@ha
	addis 11,0,block_start@ha
	lwz 9,strstart@l(9)
	lwz 0,block_start@l(11)
	subf 6,0,9
	li 11,0
	addis 9,0,dyn_dtree@ha
	addi 7,9,dyn_dtree@l
	addis 9,0,extra_dbits@ha
	addi 8,9,extra_dbits@l
.L246:
	slwi 0,11,2
	lhzx 9,7,0
	lwzx 0,8,0
	addic 0,0,5
	mullw 9,9,0
	add 10,10,9
	addi 11,11,1
	cmpwi 1,11,29
	bc 4,5,.L246
	srwi 10,10,3
	addis 11,0,last_dist@ha
	addis 9,0,last_lit@ha
	lwz 0,last_lit@l(9)
	srwi 0,0,1
	lwz 9,last_dist@l(11)
	cmplw 1,9,0
	bc 4,4,.L242
	srwi 0,6,1
	cmplw 1,10,0
	li 3,1
	bclr 12,4
.L242:
	li 3,0
	addis 9,0,last_lit@ha
	lwz 0,last_lit@l(9)
	cmpwi 1,0,32767
	bc 12,6,.L250
	addis 9,0,last_dist@ha
	lwz 9,last_dist@l(9)
	li 0,0
	ori 0,0,32768
	cmpw 1,9,0
	bclr 4,6
.L250:
	li 3,1
	blr
.Lfe12:
	.size	 ct_tally,.Lfe12-ct_tally
	.align 2
	.type	 compress_block,@function
compress_block:
	stwu 1,-72(1)
	mflr 0
	stw 16,8(1)
	stw 17,12(1)
	stw 18,16(1)
	stw 19,20(1)
	stw 20,24(1)
	stw 21,28(1)
	stw 22,32(1)
	stw 23,36(1)
	stw 24,40(1)
	stw 25,44(1)
	stw 26,48(1)
	stw 27,52(1)
	stw 28,56(1)
	stw 29,60(1)
	stw 30,64(1)
	stw 31,68(1)
	stw 0,76(1)
	mr 26,3
	mr 24,4
	li 29,0
	mr 27,29
	mr 25,29
	mr 28,29
	addis 9,0,last_lit@ha
	lwz 0,last_lit@l(9)
	cmpw 1,0,29
	bc 12,6,.L255
	addis 9,0,flag_buf@ha
	addi 16,9,flag_buf@l
	addis 9,0,inbuf@ha
	addi 17,9,inbuf@l
	addis 9,0,length_code@ha
	addi 18,9,length_code@l
	addis 9,0,extra_lbits@ha
	addi 19,9,extra_lbits@l
	addis 9,0,base_length@ha
	addi 20,9,base_length@l
	addis 9,0,d_buf@ha
	addi 21,9,d_buf@l
	addis 9,0,dist_code@ha
	addi 23,9,dist_code@l
	addis 9,0,extra_dbits@ha
	addi 22,9,extra_dbits@l
.L256:
	andi. 0,29,7
	bc 4,2,.L259
	lbzx 28,16,25
	addi 25,25,1
.L259:
	lbzx 30,17,29
	addi 29,29,1
	andi. 0,28,1
	bc 4,2,.L260
	slwi 0,30,2
	add 9,0,26
	lhzx 3,26,0
	lhz 4,2(9)
	b .L267
.L260:
	lbzx 9,18,30
	slwi 31,9,2
	add 9,31,26
	lhz 3,1028(9)
	lhz 4,1030(9)
	bl send_bits
	lwzx 4,19,31
	cmpwi 1,4,0
	bc 12,6,.L262
	lwzx 3,20,31
	subf 3,3,30
	bl send_bits
.L262:
	add 0,27,27
	lhzx 30,21,0
	addi 27,27,1
	cmplwi 1,30,255
	bc 12,5,.L263
	lbzx 9,23,30
	b .L264
.L263:
	srwi 0,30,7
	addic 0,0,256
	lbzx 9,23,0
.L264:
	slwi 31,9,2
	add 9,31,24
	lhzx 3,31,24
	lhz 4,2(9)
	bl send_bits
	lwzx 4,22,31
	cmpwi 1,4,0
	bc 12,6,.L261
	addis 9,0,base_dist@ha
	addi 9,9,base_dist@l
	lwzx 3,9,31
	subf 3,3,30
.L267:
	bl send_bits
.L261:
	srwi 28,28,1
	addis 9,0,last_lit@ha
	lwz 0,last_lit@l(9)
	cmplw 1,29,0
	bc 12,4,.L256
.L255:
	lhz 3,1024(26)
	lhz 4,1026(26)
	bl send_bits
	lwz 0,76(1)
	mtlr 0
	lwz 16,8(1)
	lwz 17,12(1)
	lwz 18,16(1)
	lwz 19,20(1)
	lwz 20,24(1)
	lwz 21,28(1)
	lwz 22,32(1)
	lwz 23,36(1)
	lwz 24,40(1)
	lwz 25,44(1)
	lwz 26,48(1)
	lwz 27,52(1)
	lwz 28,56(1)
	lwz 29,60(1)
	lwz 30,64(1)
	lwz 31,68(1)
	addi 1,1,72
	blr
.Lfe13:
	.size	 compress_block,.Lfe13-compress_block
	.section	".rodata"
	.align 2
.LC1:
	.string	"-l used on binary file"
	.align 2
.LC2:
	.string	""
	.section ".text"
	.align 2
	.type	 set_file_type,@function
set_file_type:
	li 11,0
	mr 8,11
	mr 10,11
	addis 9,0,dyn_ltree@ha
	addi 9,9,dyn_ltree@l
.L271:
	slwi 0,11,2
	lhzx 0,9,0
	add 10,10,0
	addi 11,11,1
	cmpwi 1,11,6
	bc 4,5,.L271
	cmpwi 1,11,127
	bc 12,5,.L282
	addis 9,0,dyn_ltree@ha
	addi 9,9,dyn_ltree@l
.L275:
	slwi 0,11,2
	lhzx 0,9,0
	add 8,8,0
	addi 11,11,1
	cmpwi 1,11,127
	bc 4,5,.L275
.L282:
	cmpwi 1,11,255
	bc 12,5,.L283
	addis 9,0,dyn_ltree@ha
	addi 9,9,dyn_ltree@l
.L279:
	slwi 0,11,2
	lhzx 0,9,0
	add 10,10,0
	addi 11,11,1
	cmpwi 1,11,255
	bc 4,5,.L279
.L283:
	addis 9,0,file_type@ha
	lwz 9,file_type@l(9)
	srwi 0,8,2
	subfc 0,10,0
	li 0,0
	adde 0,0,0
	sth 0,0(9)
	blr
.Lfe14:
	.size	 set_file_type,.Lfe14-set_file_type
	.lcomm	dyn_ltree,2292,2
	.lcomm	dyn_dtree,244,2
	.lcomm	static_ltree,1152,2
	.lcomm	static_dtree,120,2
	.lcomm	bl_tree,156,2
	.lcomm	bl_count,32,2
	.lcomm	heap,2292,4
	.section ".sdata","aw"
	.align 2
heap_len:
	.space	4
	.size	 heap_len,4
	.align 2
heap_max:
	.space	4
	.size	 heap_max,4
	.lcomm	depth,573,1
	.lcomm	length_code,256,1
	.lcomm	dist_code,512,1
	.lcomm	base_length,116,4
	.lcomm	base_dist,120,4
	.lcomm	flag_buf,4096,1
	.align 2
last_lit:
	.space	4
	.size	 last_lit,4
	.align 2
last_dist:
	.space	4
	.size	 last_dist,4
	.align 2
last_flags:
	.space	4
	.size	 last_flags,4
flags:
	.space	1
	.size	 flags,1
flag_bit:
	.space	1
	.size	 flag_bit,1
	.align 2
opt_len:
	.space	4
	.size	 opt_len,4
	.align 2
static_len:
	.space	4
	.size	 static_len,4
	.align 2
compressed_len:
	.space	4
	.size	 compressed_len,4
	.align 2
input_len:
	.space	4
	.size	 input_len,4
	.comm	file_type,4,4
	.comm	file_method,4,4
	.ident	"GCC: (GNU) 2.7-97r2"
