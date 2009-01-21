	.file	"unpack.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.section	".rodata"
	.align 2
.LC0:
	.string	"invalid compressed data -- Huffman code > 32 bits"
	.align 2
.LC1:
	.string	"too many leaves in Huffman tree"
	.section ".text"
	.align 2
	.type	 read_tree,@function
read_tree:
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
	addis 9,0,orig_len@ha
	li 0,0
	stw 0,orig_len@l(9)
	li 30,1
	mr 25,9
	addi 26,9,orig_len@l
	addis 29,0,inptr@ha
	addis 27,0,insize@ha
	addis 9,0,inbuf@ha
	addi 28,9,inbuf@l
.L5:
	lwz 0,orig_len@l(25)
	slwi 31,0,8
	lwz 9,inptr@l(29)
	lwz 0,insize@l(27)
	cmplw 1,9,0
	bc 4,4,.L6
	lbzx 0,28,9
	or 11,31,0
	addi 0,9,1
	stw 0,inptr@l(29)
	b .L7
.L6:
	li 3,0
	bl fill_inbuf
	or 11,31,3
.L7:
	stw 11,0(26)
	addi 30,30,1
	cmpwi 1,30,4
	bc 4,5,.L5
	addis 9,0,max_len@ha
	addi 31,9,max_len@l
	addis 10,0,inptr@ha
	addis 9,0,insize@ha
	lwz 11,inptr@l(10)
	lwz 0,insize@l(9)
	cmplw 1,11,0
	bc 4,4,.L9
	addis 9,0,inbuf@ha
	addi 9,9,inbuf@l
	lbzx 3,9,11
	addi 0,11,1
	stw 0,inptr@l(10)
	b .L10
.L9:
	li 3,0
	bl fill_inbuf
.L10:
	stw 3,0(31)
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	cmpwi 1,0,25
	bc 4,5,.L11
	addis 3,0,.LC0@ha
	addi 3,3,.LC0@l
	bl error
.L11:
	li 30,0
	li 29,1
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	cmpw 1,29,0
	bc 12,5,.L13
	addis 9,0,leaves@ha
	addi 27,9,leaves@l
	addis 28,0,inptr@ha
	addis 24,0,insize@ha
	addis 9,0,inbuf@ha
	addi 25,9,inbuf@l
	addis 26,0,max_len@ha
.L15:
	slwi 31,29,2
	lwz 9,inptr@l(28)
	lwz 0,insize@l(24)
	cmplw 1,9,0
	bc 4,4,.L16
	lbzx 3,25,9
	addi 0,9,1
	stw 0,inptr@l(28)
	b .L17
.L16:
	li 3,0
	bl fill_inbuf
.L17:
	stwx 3,27,31
	slwi 0,29,2
	lwzx 0,27,0
	add 30,30,0
	addi 29,29,1
	lwz 0,max_len@l(26)
	cmpw 1,29,0
	bc 4,5,.L15
.L13:
	cmpwi 1,30,256
	bc 4,5,.L19
	addis 3,0,.LC1@ha
	addi 3,3,.LC1@l
	bl error
.L19:
	addis 11,0,leaves@ha
	addis 9,0,max_len@ha
	lwz 10,max_len@l(9)
	addi 11,11,leaves@l
	slwi 9,10,2
	lwzx 0,11,9
	addic 0,0,1
	stwx 0,11,9
	li 28,0
	li 29,1
	cmpw 1,29,10
	bc 12,5,.L21
	addis 9,0,lit_base@ha
	addi 22,9,lit_base@l
	addis 9,0,leaves@ha
	addi 23,9,leaves@l
	addis 9,0,literal@ha
	addi 24,9,literal@l
	addis 26,0,inptr@ha
	addis 25,0,insize@ha
.L23:
	slwi 0,29,2
	stwx 28,22,0
	lwzx 30,23,0
	cmpwi 1,30,0
	bc 4,5,.L22
	addis 9,0,inbuf@ha
	addi 27,9,inbuf@l
.L27:
	mr 31,28
	addi 28,31,1
	lwz 9,inptr@l(26)
	lwz 0,insize@l(25)
	cmplw 1,9,0
	bc 4,4,.L28
	lbzx 3,27,9
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L29
.L28:
	li 3,0
	bl fill_inbuf
.L29:
	stbx 3,24,31
	addic. 30,30,-1
	bc 12,1,.L27
.L22:
	addi 29,29,1
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	cmpw 1,29,0
	bc 4,5,.L23
.L21:
	addis 11,0,leaves@ha
	addis 9,0,max_len@ha
	lwz 9,max_len@l(9)
	addi 11,11,leaves@l
	slwi 9,9,2
	lwzx 0,11,9
	addic 0,0,1
	stwx 0,11,9
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
	.size	 read_tree,.Lfe1-read_tree
	.align 2
	.type	 build_tree,@function
build_tree:
	li 11,0
	addis 9,0,max_len@ha
	lwz 10,max_len@l(9)
	cmpw 1,10,11
	bc 4,5,.L34
	addis 9,0,parents@ha
	addi 6,9,parents@l
	addis 9,0,lit_base@ha
	addi 8,9,lit_base@l
	addis 9,0,leaves@ha
	addi 7,9,leaves@l
.L36:
	srawi 11,11,1
	slwi 0,10,2
	stwx 11,6,0
	lwzx 9,8,0
	subf 9,11,9
	stwx 9,8,0
	lwzx 0,7,0
	add 11,11,0
	addic. 10,10,-1
	bc 12,1,.L36
.L34:
	addis 9,0,peek_bits@ha
	addi 11,9,peek_bits@l
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	cmpwi 1,0,12
	bc 4,5,.L39
	li 0,12
.L39:
	stw 0,0(11)
	addis 9,0,peek_bits@ha
	li 0,1
	lwz 11,peek_bits@l(9)
	slw 0,0,11
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	add 8,0,9
	li 10,1
	cmpw 1,10,11
	bc 12,5,.L41
	addis 9,0,leaves@ha
	addi 7,9,leaves@l
	addis 11,0,peek_bits@ha
.L43:
	slwi 9,10,2
	lwz 0,peek_bits@l(11)
	subf 0,10,0
	lwzx 9,7,9
	slw 9,9,0
	cmpwi 1,9,0
	addi 9,9,-1
	bc 12,6,.L42
.L46:
	stbu 10,-1(8)
	cmpwi 1,9,0
	addi 9,9,-1
	bc 4,6,.L46
.L42:
	addi 10,10,1
	lwz 0,peek_bits@l(11)
	cmpw 1,10,0
	bc 4,5,.L43
.L41:
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	cmplw 1,8,9
	bclr 4,5
	li 0,0
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
.L51:
	stbu 0,-1(8)
	cmplw 1,8,9
	bc 12,5,.L51
	blr
.Lfe2:
	.size	 build_tree,.Lfe2-build_tree
	.section	".rodata"
	.align 2
.LC2:
	.string	"invalid compressed data--length error"
	.section ".text"
	.align 2
	.globl unpack
	.type	 unpack,@function
unpack:
	stwu 1,-56(1)
	mflr 0
	stw 20,8(1)
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
	addis 9,0,ifd@ha
	stw 3,ifd@l(9)
	addis 9,0,ofd@ha
	stw 4,ofd@l(9)
	bl read_tree
	bl build_tree
	addis 9,0,valid@ha
	li 0,0
	stw 0,valid@l(9)
	addis 9,0,bitbuf@ha
	stw 0,bitbuf@l(9)
	addis 9,0,peek_bits@ha
	li 11,1
	lwz 0,peek_bits@l(9)
	slw 24,11,0
	addi 24,24,-1
	addis 11,0,leaves@ha
	addi 11,11,leaves@l
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	slwi 0,0,2
	lwzx 0,11,0
	addic 20,0,-1
	addis 28,0,valid@ha
	addis 21,0,peek_bits@ha
	addis 25,0,bitbuf@ha
	mr 9,25
	addi 22,9,bitbuf@l
	addis 26,0,inptr@ha
	addis 23,0,insize@ha
.L55:
	lwz 0,valid@l(28)
	lwz 9,peek_bits@l(21)
	cmpw 1,0,9
	bc 4,4,.L59
	addis 9,0,inbuf@ha
	addi 29,9,inbuf@l
	addis 30,0,peek_bits@ha
.L60:
	lwz 0,bitbuf@l(25)
	slwi 31,0,8
	lwz 11,inptr@l(26)
	lwz 0,insize@l(23)
	cmplw 1,11,0
	bc 4,4,.L61
	lbzx 0,29,11
	or 9,31,0
	addi 0,11,1
	stw 0,inptr@l(26)
	b .L62
.L61:
	li 3,0
	bl fill_inbuf
	or 9,31,3
.L62:
	stw 9,0(22)
	lwz 0,valid@l(28)
	addic 0,0,8
	stw 0,valid@l(28)
	lwz 9,peek_bits@l(30)
	cmpw 1,0,9
	bc 12,4,.L60
.L59:
	lwz 9,valid@l(28)
	lwz 11,peek_bits@l(21)
	subf 9,11,9
	lwz 0,bitbuf@l(25)
	srw 0,0,9
	and 6,0,24
	addis 9,0,outbuf@ha
	addi 9,9,outbuf@l
	lbzx 30,9,6
	cmpwi 1,30,0
	bc 4,5,.L64
	subf 0,30,11
	srw 6,6,0
	b .L65
.L64:
	mr 29,24
	lwz 30,peek_bits@l(21)
.L66:
	addi 30,30,1
	slwi 0,29,1
	addic 29,0,1
	lwz 0,valid@l(28)
	cmpw 1,0,30
	bc 4,4,.L70
	addis 9,0,inbuf@ha
	addi 27,9,inbuf@l
.L71:
	lwz 0,bitbuf@l(25)
	slwi 31,0,8
	lwz 9,inptr@l(26)
	lwz 0,insize@l(23)
	cmplw 1,9,0
	bc 4,4,.L72
	lbzx 0,27,9
	or 11,31,0
	addi 0,9,1
	stw 0,inptr@l(26)
	b .L73
.L72:
	li 3,0
	bl fill_inbuf
	or 11,31,3
.L73:
	stw 11,0(22)
	lwz 0,valid@l(28)
	addic 0,0,8
	stw 0,valid@l(28)
	cmpw 1,0,30
	bc 12,4,.L71
.L70:
	lwz 9,valid@l(28)
	subf 9,30,9
	lwz 0,bitbuf@l(25)
	srw 0,0,9
	and 6,0,29
	addis 9,0,parents@ha
	addi 9,9,parents@l
	slwi 0,30,2
	lwzx 0,9,0
	cmplw 1,6,0
	bc 12,4,.L66
.L65:
	cmpw 1,6,20
	bc 4,6,.L76
	addis 9,0,max_len@ha
	lwz 0,max_len@l(9)
	cmpw 1,30,0
	bc 12,6,.L56
.L76:
	addis 8,0,window@ha
	addi 8,8,window@l
	addis 7,0,outcnt@ha
	lwz 10,outcnt@l(7)
	addis 11,0,literal@ha
	addi 11,11,literal@l
	addis 9,0,lit_base@ha
	addi 9,9,lit_base@l
	slwi 0,30,2
	lwzx 0,9,0
	add 0,6,0
	lbzx 0,11,0
	stbx 0,8,10
	addi 10,10,1
	stw 10,outcnt@l(7)
	li 0,0
	ori 0,0,32768
	cmpw 1,10,0
	bc 4,6,.L77
	bl flush_window
.L77:
	lwz 0,valid@l(28)
	subf 0,30,0
	stw 0,valid@l(28)
	b .L55
.L56:
	bl flush_window
	addis 9,0,orig_len@ha
	addis 11,0,bytes_out@ha
	lwz 9,orig_len@l(9)
	lwz 0,bytes_out@l(11)
	cmpw 1,9,0
	bc 12,6,.L78
	addis 3,0,.LC2@ha
	addi 3,3,.LC2@l
	bl error
.L78:
	li 3,0
	lwz 0,60(1)
	mtlr 0
	lwz 20,8(1)
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
.Lfe3:
	.size	 unpack,.Lfe3-unpack
	.section ".sdata","aw"
	.align 2
orig_len:
	.space	4
	.size	 orig_len,4
	.align 2
max_len:
	.space	4
	.size	 max_len,4
	.lcomm	literal,256,1
	.lcomm	lit_base,104,4
	.lcomm	leaves,104,4
	.lcomm	parents,104,4
	.align 2
peek_bits:
	.space	4
	.size	 peek_bits,4
	.align 2
bitbuf:
	.space	4
	.size	 bitbuf,4
	.align 2
valid:
	.space	4
	.size	 valid,4
	.ident	"GCC: (GNU) 2.7-97r2"
