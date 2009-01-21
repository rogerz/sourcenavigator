	.file	"unlzh.c"

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
	.type	 fillbuf,@function
fillbuf:
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
	mr 31,3
	addis 9,0,bitbuf@ha
	lhz 0,bitbuf@l(9)
	slw 0,0,31
	sth 0,bitbuf@l(9)
	addis 9,0,bitcount@ha
	lwz 0,bitcount@l(9)
	cmpw 1,31,0
	bc 4,5,.L3
	addis 27,0,bitbuf@ha
	addis 30,0,subbitbuf@ha
	mr 28,9
	mr 9,30
	addi 22,9,subbitbuf@l
	addis 29,0,inptr@ha
	addis 23,0,insize@ha
	addis 9,0,inbuf@ha
	addi 24,9,inbuf@l
	li 25,0
	li 26,8
.L4:
	lwz 0,bitcount@l(28)
	subf 31,0,31
	lwz 9,subbitbuf@l(30)
	slw 9,9,31
	lhz 0,bitbuf@l(27)
	or 0,0,9
	sth 0,bitbuf@l(27)
	lwz 9,inptr@l(29)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L5
	lbzx 3,24,9
	addi 0,9,1
	stw 0,inptr@l(29)
	b .L6
.L5:
	li 3,1
	bl fill_inbuf
.L6:
	stw 3,0(22)
	lwz 0,subbitbuf@l(30)
	cmpwi 1,0,-1
	bc 4,6,.L7
	stw 25,subbitbuf@l(30)
.L7:
	stw 26,bitcount@l(28)
	cmpwi 1,31,8
	bc 12,5,.L4
.L3:
	addis 10,0,bitbuf@ha
	addis 11,0,subbitbuf@ha
	addis 9,0,bitcount@ha
	lwz 0,bitcount@l(9)
	subf 0,31,0
	stw 0,bitcount@l(9)
	lwz 9,subbitbuf@l(11)
	srw 9,9,0
	lhz 0,bitbuf@l(10)
	or 0,0,9
	sth 0,bitbuf@l(10)
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
.Lfe1:
	.size	 fillbuf,.Lfe1-fillbuf
	.align 2
	.type	 getbits,@function
getbits:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 9,0,bitbuf@ha
	lhz 29,bitbuf@l(9)
	subfic 0,3,16
	sraw 29,29,0
	bl fillbuf
	mr 3,29
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe2:
	.size	 getbits,.Lfe2-getbits
	.align 2
	.type	 init_getbits,@function
init_getbits:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	addis 9,0,bitbuf@ha
	li 0,0
	sth 0,bitbuf@l(9)
	addis 9,0,subbitbuf@ha
	li 0,0
	stw 0,subbitbuf@l(9)
	addis 9,0,bitcount@ha
	stw 0,bitcount@l(9)
	li 3,16
	bl fillbuf
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe3:
	.size	 init_getbits,.Lfe3-init_getbits
	.section	".rodata"
	.align 2
.LC0:
	.string	"Bad table\n"
	.section ".text"
	.align 2
	.type	 make_table,@function
make_table:
	stwu 1,-160(1)
	mflr 0
	stw 25,132(1)
	stw 26,136(1)
	stw 27,140(1)
	stw 28,144(1)
	stw 29,148(1)
	stw 30,152(1)
	stw 31,156(1)
	stw 0,164(1)
	mr 29,3
	mr 25,4
	mr 31,5
	mr 30,6
	li 7,1
	li 11,0
	addi 9,1,8
.L15:
	add 0,7,7
	sthx 11,9,0
	addi 7,7,1
	cmplwi 1,7,16
	bc 4,5,.L15
	li 7,0
	cmplw 1,7,29
	bc 4,4,.L18
	addi 11,1,8
.L20:
	lbzx 0,25,7
	add 0,0,0
	lhzx 9,11,0
	addi 9,9,1
	sthx 9,11,0
	addi 7,7,1
	cmplw 1,7,29
	bc 12,4,.L20
.L18:
	li 0,0
	sth 0,90(1)
	li 7,1
	addi 6,1,8
.L25:
	addi 10,7,1
	add 11,10,10
	add 11,6,11
	add 0,7,7
	add 8,6,0
	lhzx 9,6,0
	subfic 0,7,16
	slw 9,9,0
	lhz 0,80(8)
	add 0,0,9
	sth 0,80(11)
	mr 7,10
	cmplwi 1,7,16
	bc 4,5,.L25
	lhz 0,122(1)
	cmpwi 1,0,0
	bc 12,6,.L27
	addis 3,0,.LC0@ha
	addi 3,3,.LC0@l
	bl error
.L27:
	subfic 26,31,16
	li 7,1
	cmplw 1,7,31
	bc 12,5,.L29
	addi 11,1,8
	mr 10,7
.L31:
	add 9,7,7
	add 9,11,9
	lhz 0,80(9)
	sraw 0,0,26
	sth 0,80(9)
	subf 0,7,31
	slw 0,10,0
	sth 0,40(9)
	addi 7,7,1
	cmplw 1,7,31
	bc 4,5,.L31
.L29:
	cmplwi 1,7,16
	bc 12,5,.L62
	addi 11,1,8
	li 10,1
.L35:
	add 9,7,7
	add 9,11,9
	subfic 0,7,16
	slw 0,10,0
	sth 0,40(9)
	addi 7,7,1
	cmplwi 1,7,16
	bc 4,5,.L35
.L62:
	addi 0,31,1
	add 0,0,0
	add 9,1,0
	lhz 7,88(9)
	sraw. 7,7,26
	bc 12,2,.L37
	li 0,1
	slw 10,0,31
	cmpw 1,7,10
	bc 12,6,.L37
	li 9,0
.L40:
	add 0,7,7
	sthx 9,30,0
	addi 7,7,1
	cmpw 1,7,10
	bc 4,6,.L40
.L37:
	mr 6,29
	subfic 0,31,15
	li 9,1
	slw 12,9,0
	li 5,0
	cmplw 1,5,29
	bc 4,4,.L43
	addi 3,1,8
	addis 9,0,prev+65536@ha
	addi 27,9,prev+65536@l
	addis 9,0,prev@ha
	addi 28,9,prev@l
.L45:
	lbzx 8,25,5
	cmpwi 1,8,0
	bc 12,6,.L44
	add 9,8,8
	add 9,3,9
	lhz 10,80(9)
	lhz 0,40(9)
	add 4,10,0
	cmplw 1,8,31
	bc 12,5,.L47
	mr 7,10
	cmplw 1,7,4
	bc 4,4,.L53
.L51:
	add 0,7,7
	sthx 5,30,0
	addi 7,7,1
	cmplw 1,7,4
	bc 12,4,.L51
	b .L53
.L47:
	add 9,8,8
	add 9,3,9
	lhz 10,80(9)
	srw 0,10,26
	add 0,0,0
	add 11,30,0
	subf. 7,31,8
	bc 12,2,.L55
.L56:
	lhz 9,0(11)
	cmpwi 1,9,0
	bc 4,6,.L57
	add 0,6,6
	sthx 9,28,0
	sthx 9,27,0
	sth 6,0(11)
	addi 6,6,1
.L57:
	and. 0,10,12
	bc 12,2,.L58
	lhz 0,0(11)
	add 0,0,0
	add 11,0,27
	b .L59
.L58:
	lhz 0,0(11)
	add 0,0,0
	add 11,0,28
.L59:
	slwi 10,10,1
	addic. 7,7,-1
	bc 4,2,.L56
.L55:
	sth 5,0(11)
.L53:
	add 9,8,8
	add 9,3,9
	sth 4,80(9)
.L44:
	addi 5,5,1
	cmplw 1,5,29
	bc 12,4,.L45
.L43:
	lwz 0,164(1)
	mtlr 0
	lwz 25,132(1)
	lwz 26,136(1)
	lwz 27,140(1)
	lwz 28,144(1)
	lwz 29,148(1)
	lwz 30,152(1)
	lwz 31,156(1)
	addi 1,1,160
	blr
.Lfe4:
	.size	 make_table,.Lfe4-make_table
	.align 2
	.type	 read_pt_len,@function
read_pt_len:
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
	mr 31,4
	mr 25,5
	mr 3,31
	bl getbits
	mr. 28,3
	bc 4,2,.L64
	mr 3,31
	bl getbits
	mr 31,3
	li 30,0
	cmpw 1,30,29
	bc 4,4,.L66
	addis 9,0,pt_len@ha
	addi 9,9,pt_len@l
	mr 0,30
.L68:
	stbx 0,9,30
	addi 30,30,1
	cmpw 1,30,29
	bc 12,4,.L68
.L66:
	li 30,0
	addis 9,0,pt_table@ha
	addi 9,9,pt_table@l
.L73:
	add 0,30,30
	sthx 31,9,0
	addi 30,30,1
	cmpwi 1,30,255
	bc 4,5,.L73
	b .L75
.L64:
	li 30,0
	cmpw 1,30,28
	bc 4,4,.L77
	addis 24,0,bitbuf@ha
	addis 9,0,pt_len@ha
	addi 27,9,pt_len@l
	mr 26,30
.L78:
	lhz 9,bitbuf@l(24)
	srwi 31,9,13
	cmpwi 1,31,7
	bc 4,6,.L79
	li 11,4096
	andi. 0,9,4096
	bc 12,2,.L79
	addis 9,0,bitbuf@ha
	lhz 9,bitbuf@l(9)
.L82:
	srwi 11,11,1
	addi 31,31,1
	and. 0,11,9
	bc 4,2,.L82
.L79:
	cmpwi 1,31,7
	mfcr 0
	rlwinm 0,0,5,1
	neg 0,0
	addi 9,31,-3
	rlwinm 3,0,0,30,31
	andc 0,9,0
	or 3,3,0
	bl fillbuf
	stbx 31,27,30
	addi 30,30,1
	cmpw 1,30,25
	bc 4,6,.L76
	li 3,2
	bl getbits
	addic. 31,3,-1
	bc 12,0,.L76
.L89:
	stbx 26,27,30
	addi 30,30,1
	addic. 31,31,-1
	bc 4,0,.L89
.L76:
	cmpw 1,30,28
	bc 12,4,.L78
.L77:
	cmpw 1,30,29
	bc 4,4,.L96
	addis 9,0,pt_len@ha
	addi 9,9,pt_len@l
	li 0,0
.L94:
	stbx 0,9,30
	addi 30,30,1
	cmpw 1,30,29
	bc 12,4,.L94
.L96:
	mr 3,29
	addis 4,0,pt_len@ha
	addi 4,4,pt_len@l
	li 5,8
	addis 6,0,pt_table@ha
	addi 6,6,pt_table@l
	bl make_table
.L75:
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
.Lfe5:
	.size	 read_pt_len,.Lfe5-read_pt_len
	.align 2
	.type	 read_c_len,@function
read_c_len:
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
	li 3,9
	bl getbits
	mr. 29,3
	bc 4,2,.L98
	li 3,9
	bl getbits
	mr 31,3
	li 30,0
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	mr 0,30
.L102:
	stbx 0,9,30
	addi 30,30,1
	cmpwi 1,30,509
	bc 4,5,.L102
	li 30,0
	addis 9,0,d_buf@ha
	addi 9,9,d_buf@l
.L107:
	add 0,30,30
	sthx 31,9,0
	addi 30,30,1
	cmpwi 1,30,4095
	bc 4,5,.L107
	b .L109
.L98:
	li 30,0
	cmpw 1,30,29
	bc 4,4,.L111
	addis 9,0,pt_table@ha
	addi 25,9,pt_table@l
	addis 28,0,bitbuf@ha
	addis 9,0,prev+65536@ha
	addi 26,9,prev+65536@l
	addis 9,0,prev@ha
	addi 27,9,prev@l
.L112:
	lhz 0,bitbuf@l(28)
	rlwinm 0,0,25,23,30
	lhzx 31,25,0
	cmpwi 1,31,18
	bc 4,5,.L113
	li 9,128
	lhz 11,bitbuf@l(28)
.L114:
	and. 0,11,9
	bc 12,2,.L117
	add 0,31,31
	lhzx 31,26,0
	b .L118
.L117:
	add 0,31,31
	lhzx 31,27,0
.L118:
	srwi 9,9,1
	cmpwi 1,31,18
	bc 12,5,.L114
.L113:
	addis 9,0,pt_len@ha
	addi 9,9,pt_len@l
	lbzx 3,9,31
	bl fillbuf
	cmpwi 1,31,2
	bc 12,5,.L120
	cmpwi 1,31,0
	bc 4,6,.L121
	li 31,1
	b .L122
.L121:
	cmpwi 1,31,1
	bc 4,6,.L123
	li 3,4
	bl getbits
	addi 31,3,3
	b .L122
.L123:
	li 3,9
	bl getbits
	addi 31,3,20
.L122:
	addic. 31,31,-1
	bc 12,0,.L110
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	li 0,0
.L127:
	stbx 0,9,30
	addi 30,30,1
	addic. 31,31,-1
	bc 4,0,.L127
	b .L110
.L120:
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	addi 0,31,-2
	stbx 0,9,30
	addi 30,30,1
.L110:
	cmpw 1,30,29
	bc 12,4,.L112
.L111:
	cmpwi 1,30,509
	bc 12,5,.L135
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	li 0,0
.L133:
	stbx 0,9,30
	addi 30,30,1
	cmpwi 1,30,509
	bc 4,5,.L133
.L135:
	li 3,510
	addis 4,0,outbuf@ha
	addi 4,4,outbuf@l
	li 5,12
	addis 6,0,d_buf@ha
	addi 6,6,d_buf@l
	bl make_table
.L109:
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
.Lfe6:
	.size	 read_c_len,.Lfe6-read_c_len
	.align 2
	.type	 decode_c,@function
decode_c:
	stwu 1,-16(1)
	mflr 0
	stw 31,12(1)
	stw 0,20(1)
	addis 31,0,blocksize@ha
	lwz 0,blocksize@l(31)
	cmpwi 1,0,0
	bc 4,6,.L137
	li 3,16
	bl getbits
	stw 3,blocksize@l(31)
	cmpwi 1,3,0
	li 3,510
	bc 12,6,.L146
	li 3,19
	li 4,5
	li 5,3
	bl read_pt_len
	bl read_c_len
	li 3,14
	li 4,4
	li 5,-1
	bl read_pt_len
.L137:
	addis 9,0,blocksize@ha
	lwz 0,blocksize@l(9)
	addic 0,0,-1
	stw 0,blocksize@l(9)
	addis 11,0,d_buf@ha
	addi 11,11,d_buf@l
	addis 9,0,bitbuf@ha
	lhz 0,bitbuf@l(9)
	rlwinm 0,0,29,19,30
	lhzx 31,11,0
	cmplwi 1,31,509
	bc 4,5,.L139
	li 11,8
	lhz 10,bitbuf@l(9)
	addis 9,0,prev+65536@ha
	addi 8,9,prev+65536@l
	addis 9,0,prev@ha
	addi 9,9,prev@l
.L140:
	and. 0,10,11
	bc 12,2,.L143
	add 0,31,31
	lhzx 31,8,0
	b .L144
.L143:
	add 0,31,31
	lhzx 31,9,0
.L144:
	srwi 11,11,1
	cmplwi 1,31,509
	bc 12,5,.L140
.L139:
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	lbzx 3,9,31
	bl fillbuf
	mr 3,31
.L146:
	lwz 0,20(1)
	mtlr 0
	lwz 31,12(1)
	addi 1,1,16
	blr
.Lfe7:
	.size	 decode_c,.Lfe7-decode_c
	.align 2
	.type	 decode_p,@function
decode_p:
	stwu 1,-24(1)
	mflr 0
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	addis 11,0,pt_table@ha
	addi 11,11,pt_table@l
	addis 9,0,bitbuf@ha
	lhz 0,bitbuf@l(9)
	rlwinm 0,0,25,23,30
	lhzx 29,11,0
	cmplwi 1,29,13
	bc 4,5,.L148
	li 11,128
	lhz 10,bitbuf@l(9)
	addis 9,0,prev+65536@ha
	addi 8,9,prev+65536@l
	addis 9,0,prev@ha
	addi 9,9,prev@l
.L149:
	and. 0,10,11
	bc 12,2,.L152
	add 0,29,29
	lhzx 29,8,0
	b .L153
.L152:
	add 0,29,29
	lhzx 29,9,0
.L153:
	srwi 11,11,1
	cmplwi 1,29,13
	bc 12,5,.L149
.L148:
	addis 9,0,pt_len@ha
	addi 9,9,pt_len@l
	lbzx 3,9,29
	bl fillbuf
	cmpwi 1,29,0
	bc 12,6,.L155
	addi 29,29,-1
	mr 3,29
	bl getbits
	li 0,1
	slw 29,0,29
	add 29,29,3
.L155:
	mr 3,29
	lwz 0,28(1)
	mtlr 0
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe8:
	.size	 decode_p,.Lfe8-decode_p
	.align 2
	.type	 huf_decode_start,@function
huf_decode_start:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	bl init_getbits
	addis 9,0,blocksize@ha
	li 0,0
	stw 0,blocksize@l(9)
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe9:
	.size	 huf_decode_start,.Lfe9-huf_decode_start
	.align 2
	.type	 decode_start,@function
decode_start:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	bl huf_decode_start
	addis 9,0,j@ha
	li 0,0
	stw 0,j@l(9)
	addis 9,0,done@ha
	stw 0,done@l(9)
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe10:
	.size	 decode_start,.Lfe10-decode_start
	.section ".sdata","aw"
	.align 2
i.22:
	.space	4
	.size	 i.22,4
	.section ".text"
	.align 2
	.type	 decode,@function
decode:
	stwu 1,-24(1)
	mflr 0
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 28,3
	mr 30,4
	li 31,0
	addis 9,0,j@ha
	lwz 0,j@l(9)
	addic 0,0,-1
	stw 0,j@l(9)
	cmpw 1,0,31
	bc 12,4,.L164
	addis 11,0,i.22@ha
.L161:
	lwz 0,i.22@l(11)
	lbzx 0,30,0
	stbx 0,30,31
	lwz 0,i.22@l(11)
	addic 0,0,1
	rlwinm 0,0,0,19,31
	stw 0,i.22@l(11)
	addi 31,31,1
	cmpw 1,31,28
	bc 12,6,.L171
	lwz 0,j@l(9)
	addic 0,0,-1
	stw 0,j@l(9)
	cmpwi 1,0,0
	bc 4,4,.L161
.L164:
	bl decode_c
	cmpwi 1,3,510
	bc 4,6,.L167
	addis 9,0,done@ha
	li 0,1
	stw 0,done@l(9)
	b .L171
.L167:
	cmplwi 1,3,255
	bc 12,5,.L168
	stbx 3,30,31
	addi 31,31,1
	cmpw 1,31,28
	bc 4,6,.L164
	b .L171
.L168:
	addis 9,0,j@ha
	addi 0,3,-253
	stw 0,j@l(9)
	addis 29,0,i.22@ha
	bl decode_p
	subf 3,3,31
	addi 3,3,-1
	rlwinm 3,3,0,19,31
	stw 3,i.22@l(29)
	addis 9,0,j@ha
	lwz 0,j@l(9)
	addic 0,0,-1
	stw 0,j@l(9)
	cmpwi 1,0,0
	bc 12,4,.L164
	mr 11,29
.L173:
	lwz 0,i.22@l(11)
	lbzx 0,30,0
	stbx 0,30,31
	lwz 0,i.22@l(11)
	addic 0,0,1
	rlwinm 0,0,0,19,31
	stw 0,i.22@l(11)
	addi 31,31,1
	cmpw 1,31,28
	bc 12,6,.L171
	lwz 0,j@l(9)
	addic 0,0,-1
	stw 0,j@l(9)
	cmpwi 1,0,0
	bc 4,4,.L173
	b .L164
.L171:
	mr 3,31
	lwz 0,28(1)
	mtlr 0
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe11:
	.size	 decode,.Lfe11-decode
	.align 2
	.globl unlzh
	.type	 unlzh,@function
unlzh:
	stwu 1,-24(1)
	mflr 0
	stw 28,8(1)
	stw 29,12(1)
	stw 30,16(1)
	stw 31,20(1)
	stw 0,28(1)
	mr 31,4
	addis 9,0,ifd@ha
	stw 3,ifd@l(9)
	addis 9,0,ofd@ha
	stw 31,ofd@l(9)
	bl decode_start
	addis 9,0,done@ha
	lwz 0,done@l(9)
	cmpwi 1,0,0
	bc 4,6,.L180
	addis 30,0,window@ha
	addis 28,0,test@ha
	mr 29,9
.L181:
	li 3,8192
	addi 4,30,window@l
	bl decode
	mr 5,3
	lwz 0,test@l(28)
	subfic 9,0,0
	adde 0,9,0
	addic 9,5,-1
	subfe 11,9,5
	and. 9,0,11
	bc 12,2,.L179
	mr 3,31
	addi 4,30,window@l
	bl write_buf
.L179:
	lwz 0,done@l(29)
	cmpwi 1,0,0
	bc 12,6,.L181
.L180:
	li 3,0
	lwz 0,28(1)
	mtlr 0
	lwz 28,8(1)
	lwz 29,12(1)
	lwz 30,16(1)
	lwz 31,20(1)
	addi 1,1,24
	blr
.Lfe12:
	.size	 unlzh,.Lfe12-unlzh
	.lcomm	pt_len,19,1
	.section ".sdata","aw"
	.align 2
blocksize:
	.space	4
	.size	 blocksize,4
	.lcomm	pt_table,512,2
	.align 1
bitbuf:
	.space	2
	.size	 bitbuf,2
	.align 2
subbitbuf:
	.space	4
	.size	 subbitbuf,4
	.align 2
bitcount:
	.space	4
	.size	 bitcount,4
	.align 2
j:
	.space	4
	.size	 j,4
	.align 2
done:
	.space	4
	.size	 done,4
	.ident	"GCC: (GNU) 2.7-97r2"
