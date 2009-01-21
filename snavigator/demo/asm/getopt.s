	.file	"getopt.c"

 # rs6000/powerpc options: -msdata=data -G 8
 # GNU C version 2.7-97r2 (powerpc-eabi) compiled by GNU C version 2.7-97r2.
 # options passed:  -O
 # options enabled:  -fdefer-pop -fomit-frame-pointer -fthread-jumps
 # -fpeephole -ffunction-cse -finline -fkeep-static-consts
 # -fpcc-struct-return -fsjlj-exceptions -fcommon -fverbose-asm
 # -fgnu-linker -mpowerpc -mnew-mnemonics -meabi -mcall-sysv -msdata=data

gcc2_compiled.:
	.globl optarg
	.section ".sdata","aw"
	.align 2
	.type	 optarg,@object
	.size	 optarg,4
optarg:
	.long 0
	.globl optind
	.align 2
	.type	 optind,@object
	.size	 optind,4
optind:
	.long 0
	.globl opterr
	.align 2
	.type	 opterr,@object
	.size	 opterr,4
opterr:
	.long 1
	.globl optopt
	.align 2
	.type	 optopt,@object
	.size	 optopt,4
optopt:
	.long 0
	.section ".text"
	.align 2
	.type	 my_strlen,@function
my_strlen:
	li 9,0
	lbz 0,0(3)
	addi 3,3,1
	cmpw 1,0,9
	bc 12,6,.L3
.L4:
	addi 9,9,1
	lbz 0,0(3)
	addi 3,3,1
	cmpwi 1,0,0
	bc 4,6,.L4
.L3:
	mr 3,9
	blr
.Lfe1:
	.size	 my_strlen,.Lfe1-my_strlen
	.align 2
	.type	 my_index,@function
my_index:
	lbz 0,0(3)
	mr 9,0
	cmpwi 1,0,0
	bc 12,6,.L8
.L9:
	mr 0,9
	cmpw 1,0,4
	bclr 12,6
	lbzu 9,1(3)
	cmpwi 1,9,0
	bc 4,6,.L9
.L8:
	li 3,0
	blr
.Lfe2:
	.size	 my_index,.Lfe2-my_index
	.align 2
	.type	 exchange,@function
exchange:
	addis 9,0,first_nonopt@ha
	lwz 0,first_nonopt@l(9)
	slwi 0,0,2
	add 7,3,0
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	addic 0,0,-4
	add 9,3,0
	cmplw 1,7,9
	bc 4,4,.L15
.L16:
	lwz 11,0(7)
	lwz 0,0(9)
	stw 0,0(7)
	stw 11,0(9)
	addi 7,7,4
	addi 9,9,-4
	cmplw 1,7,9
	bc 12,4,.L16
.L15:
	addis 8,0,first_nonopt@ha
	lwz 10,first_nonopt@l(8)
	slwi 0,10,2
	add 7,3,0
	addis 9,0,optind@ha
	addis 11,0,last_nonopt@ha
	lwz 0,optind@l(9)
	lwz 9,last_nonopt@l(11)
	subf 0,9,0
	add 0,0,10
	stw 0,first_nonopt@l(8)
	slwi 0,0,2
	addic 0,0,-4
	add 9,3,0
	cmplw 1,7,9
	bc 4,4,.L19
.L20:
	lwz 11,0(7)
	lwz 0,0(9)
	stw 0,0(7)
	stw 11,0(9)
	addi 7,7,4
	addi 9,9,-4
	cmplw 1,7,9
	bc 12,4,.L20
.L19:
	addis 9,0,first_nonopt@ha
	lwz 0,first_nonopt@l(9)
	slwi 0,0,2
	add 7,3,0
	addis 11,0,last_nonopt@ha
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	stw 0,last_nonopt@l(11)
	slwi 0,0,2
	addic 0,0,-4
	add 9,3,0
	cmplw 1,7,9
	bclr 4,4
.L24:
	lwz 11,0(7)
	lwz 0,0(9)
	stw 0,0(7)
	stw 11,0(9)
	addi 7,7,4
	addi 9,9,-4
	cmplw 1,7,9
	bc 12,4,.L24
	blr
.Lfe3:
	.size	 exchange,.Lfe3-exchange
	.section	".rodata"
	.align 2
.LC0:
	.string	"POSIXLY_CORRECT"
	.align 2
.LC1:
	.string	"--"
	.align 2
.LC2:
	.string	"%s: option `%s' is ambiguous\n"
	.align 2
.LC3:
	.string	"%s: option `--%s' doesn't allow an argument\n"
	.align 2
.LC4:
	.string	"%s: option `%c%s' doesn't allow an argument\n"
	.align 2
.LC5:
	.string	"%s: option `%s' requires an argument\n"
	.align 2
.LC6:
	.string	"%s: unrecognized option `--%s'\n"
	.align 2
.LC7:
	.string	"%s: unrecognized option `%c%s'\n"
	.align 2
.LC8:
	.string	""
	.align 2
.LC9:
	.string	"%s: illegal option -- %c\n"
	.align 2
.LC10:
	.string	"%s: option requires an argument -- %c\n"
	.section ".text"
	.align 2
	.globl _getopt_internal
	.type	 _getopt_internal,@function
_getopt_internal:
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
	mr 21,3
	mr 26,4
	mr 22,5
	mr 29,6
	mr 18,7
	mr 19,8
	addis 9,0,optarg@ha
	li 0,0
	stw 0,optarg@l(9)
	addis 8,0,optind@ha
	lwz 10,optind@l(8)
	cmpw 1,10,0
	bc 4,6,.L27
	addis 11,0,first_nonopt@ha
	addis 9,0,last_nonopt@ha
	li 0,1
	stw 0,optind@l(8)
	stw 0,last_nonopt@l(9)
	stw 0,first_nonopt@l(11)
	addis 9,0,nextchar@ha
	stw 10,nextchar@l(9)
	lbz 0,0(22)
	cmpwi 1,0,45
	bc 4,6,.L28
	addis 9,0,ordering@ha
	li 0,2
	b .L112
.L28:
	lbz 0,0(22)
	cmpwi 1,0,43
	bc 4,6,.L30
	addis 9,0,ordering@ha
	li 0,0
.L112:
	stw 0,ordering@l(9)
	addi 22,22,1
	b .L27
.L30:
	addis 3,0,.LC0@ha
	addi 3,3,.LC0@l
	bl getenv
	cmpwi 1,3,0
	bc 12,6,.L32
	addis 9,0,ordering@ha
	li 0,0
	b .L113
.L32:
	addis 9,0,ordering@ha
	li 0,1
.L113:
	stw 0,ordering@l(9)
.L27:
	addis 9,0,nextchar@ha
	lwz 9,nextchar@l(9)
	cmpwi 1,9,0
	bc 12,6,.L35
	lbz 0,0(9)
	cmpwi 1,0,0
	bc 4,6,.L34
.L35:
	addis 9,0,ordering@ha
	lwz 0,ordering@l(9)
	cmpwi 1,0,1
	bc 4,6,.L36
	addis 9,0,first_nonopt@ha
	addis 11,0,last_nonopt@ha
	lwz 0,first_nonopt@l(9)
	lwz 11,last_nonopt@l(11)
	cmpw 1,0,11
	bc 12,6,.L37
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	cmpw 1,11,0
	bc 12,6,.L38
	mr 3,26
	bl exchange
	b .L38
.L37:
	addis 9,0,last_nonopt@ha
	addis 11,0,optind@ha
	lwz 0,last_nonopt@l(9)
	lwz 11,optind@l(11)
	cmpw 1,0,11
	bc 12,6,.L38
	addis 9,0,first_nonopt@ha
	stw 11,first_nonopt@l(9)
.L38:
	addis 11,0,optind@ha
	b .L40
.L42:
	lwz 0,optind@l(11)
	addic 0,0,1
	stw 0,optind@l(11)
.L40:
	lwz 0,optind@l(11)
	cmpw 1,0,21
	bc 4,4,.L41
	slwi 0,0,2
	lwzx 9,26,0
	lbz 0,0(9)
	cmpwi 1,0,45
	bc 4,6,.L42
	lbz 0,1(9)
	cmpwi 1,0,0
	bc 12,6,.L42
.L41:
	addis 9,0,last_nonopt@ha
	addis 11,0,optind@ha
	lwz 0,optind@l(11)
	stw 0,last_nonopt@l(9)
.L36:
	addis 31,0,optind@ha
	lwz 0,optind@l(31)
	cmpw 1,0,21
	bc 12,6,.L109
	slwi 0,0,2
	lwzx 3,26,0
	addis 4,0,.LC1@ha
	addi 4,4,.LC1@l
	bl strcmp
	cmpwi 1,3,0
	bc 4,6,.L45
	lwz 0,optind@l(31)
	addic 10,0,1
	stw 10,optind@l(31)
	addis 9,0,first_nonopt@ha
	addis 11,0,last_nonopt@ha
	lwz 0,first_nonopt@l(9)
	lwz 9,last_nonopt@l(11)
	cmpw 1,0,9
	bc 12,6,.L110
	cmpw 1,9,10
	bc 12,6,.L46
	mr 3,26
	bl exchange
	b .L47
.L46:
	addis 9,0,first_nonopt@ha
	addis 11,0,last_nonopt@ha
	lwz 9,first_nonopt@l(9)
	lwz 0,last_nonopt@l(11)
	cmpw 1,9,0
	bc 4,6,.L47
.L110:
	addis 9,0,first_nonopt@ha
	addis 11,0,optind@ha
	lwz 0,optind@l(11)
	stw 0,first_nonopt@l(9)
.L47:
	addis 9,0,last_nonopt@ha
	stw 21,last_nonopt@l(9)
	addis 9,0,optind@ha
	stw 21,optind@l(9)
.L45:
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	cmpw 1,0,21
	bc 4,6,.L49
.L109:
	addis 9,0,first_nonopt@ha
	addis 11,0,last_nonopt@ha
	lwz 10,first_nonopt@l(9)
	lwz 0,last_nonopt@l(11)
	cmpw 1,10,0
	bc 12,6,.L114
	addis 9,0,optind@ha
	stw 10,optind@l(9)
	b .L114
.L49:
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwzx 9,26,0
	lbz 0,0(9)
	cmpwi 1,0,45
	bc 4,6,.L52
	lbz 0,1(9)
	cmpwi 1,0,0
	bc 4,6,.L51
.L52:
	addis 9,0,ordering@ha
	lwz 0,ordering@l(9)
	cmpwi 1,0,0
	bc 4,6,.L53
.L114:
	li 3,-1
	b .L108
.L53:
	addis 10,0,optarg@ha
	addis 11,0,optind@ha
	lwz 0,optind@l(11)
	slwi 9,0,2
	lwzx 9,9,26
	stw 9,optarg@l(10)
	addic 0,0,1
	stw 0,optind@l(11)
	li 3,1
	b .L108
.L111:
	mr 27,31
	mr 24,28
	li 20,1
	b .L61
.L51:
	addis 9,0,nextchar@ha
	addi 10,9,nextchar@l
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 9,0,2
	add 11,9,26
	li 0,0
	cmpw 1,29,0
	bc 12,6,.L54
	lwzx 9,9,26
	lbz 0,1(9)
	xori 0,0,45
	subfic 9,0,0
	adde 0,9,0
.L54:
	addic 0,0,1
	lwz 9,0(11)
	add 0,0,9
	stw 0,0(10)
.L34:
	cmpwi 1,29,0
	bc 12,6,.L55
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwzx 9,26,0
	lbz 0,0(9)
	cmpwi 1,0,45
	bc 4,6,.L55
	lbz 0,1(9)
	xori 0,0,45
	subfic 9,0,0
	adde 0,9,0
	addic 9,19,-1
	subfe 11,9,19
	or. 9,0,11
	bc 12,2,.L55
	addis 9,0,nextchar@ha
	lwz 30,nextchar@l(9)
	li 20,0
	mr 23,20
	mr 27,20
	mr 24,20
	lbz 0,0(30)
	b .L115
.L58:
	lbzu 0,1(30)
.L115:
	neg 9,0
	srwi 9,9,31
	xori 0,0,61
	neg 0,0
	srwi 0,0,31
	and. 11,9,0
	bc 4,2,.L58
	mr 31,29
	li 28,0
	lwz 0,0(31)
	cmpw 1,0,28
	bc 12,6,.L61
	addis 25,0,nextchar@ha
.L63:
	lwz 5,nextchar@l(25)
	lwz 3,0(31)
	mr 4,5
	subf 5,5,30
	bl strncmp
	cmpwi 1,3,0
	bc 4,6,.L62
	lwz 29,nextchar@l(25)
	subf 29,29,30
	lwz 3,0(31)
	bl my_strlen
	cmpw 1,29,3
	bc 12,6,.L111
	cmpwi 1,27,0
	bc 4,6,.L67
	mr 27,31
	mr 24,28
	b .L62
.L67:
	li 23,1
.L62:
	addi 28,28,1
	lwzu 0,16(31)
	cmpwi 1,0,0
	bc 4,6,.L63
.L61:
	xori 0,20,1
	and. 9,23,0
	bc 12,2,.L70
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L71
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwz 3,12(11)
	addis 4,0,.LC2@ha
	addi 4,4,.LC2@l
	lwz 5,0(26)
	lwzx 6,26,0
	crxor 6,6,6
	bl fprintf
.L71:
	addis 29,0,nextchar@ha
	lwz 3,nextchar@l(29)
	bl my_strlen
	lwz 0,nextchar@l(29)
	add 3,3,0
	stw 3,nextchar@l(29)
	b .L116
.L70:
	cmpwi 1,27,0
	bc 12,6,.L72
	mr 28,24
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	addic 0,0,1
	stw 0,optind@l(9)
	lbz 0,0(30)
	cmpwi 1,0,0
	bc 12,6,.L73
	lwz 0,4(27)
	cmpwi 1,0,0
	bc 12,6,.L74
	addis 9,0,optarg@ha
	addi 0,30,1
	stw 0,optarg@l(9)
	b .L79
.L74:
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L76
	addis 9,0,optind@ha
	lwz 9,optind@l(9)
	slwi 9,9,2
	add 9,9,26
	lwz 9,-4(9)
	lbz 0,1(9)
	cmpwi 1,0,45
	bc 4,6,.L77
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC3@ha
	addi 4,4,.LC3@l
	lwz 5,0(26)
	lwz 6,0(27)
	crxor 6,6,6
	bl fprintf
	b .L76
.L77:
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,optind@ha
	lwz 9,optind@l(9)
	slwi 9,9,2
	add 9,9,26
	lwz 9,-4(9)
	lwz 3,12(11)
	addis 4,0,.LC4@ha
	addi 4,4,.LC4@l
	lwz 5,0(26)
	lbz 6,0(9)
	lwz 7,0(27)
	crxor 6,6,6
	bl fprintf
.L76:
	addis 29,0,nextchar@ha
	lwz 3,nextchar@l(29)
	bl my_strlen
	lwz 0,nextchar@l(29)
	add 3,3,0
	stw 3,nextchar@l(29)
	li 3,0
	b .L108
.L73:
	lwz 0,4(27)
	cmpwi 1,0,1
	bc 4,6,.L79
	addis 10,0,optind@ha
	lwz 11,optind@l(10)
	cmpw 1,11,21
	bc 4,4,.L81
	addis 9,0,optarg@ha
	slwi 0,11,2
	lwzx 0,26,0
	stw 0,optarg@l(9)
	addi 0,11,1
	stw 0,optind@l(10)
	b .L79
.L81:
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L83
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,optind@ha
	lwz 9,optind@l(9)
	slwi 9,9,2
	add 9,9,26
	lwz 3,12(11)
	addis 4,0,.LC5@ha
	addi 4,4,.LC5@l
	lwz 5,0(26)
	lwz 6,-4(9)
	crxor 6,6,6
	bl fprintf
.L83:
	addis 29,0,nextchar@ha
	lwz 3,nextchar@l(29)
	bl my_strlen
	lwz 0,nextchar@l(29)
	add 3,3,0
	stw 3,nextchar@l(29)
	lbz 0,0(22)
	xori 3,0,58
	addic 3,3,-1
	subfe 3,3,3
	andi. 3,3,58
	b .L108
.L79:
	addis 29,0,nextchar@ha
	lwz 3,nextchar@l(29)
	bl my_strlen
	lwz 0,nextchar@l(29)
	add 3,3,0
	stw 3,nextchar@l(29)
	cmpwi 1,18,0
	bc 12,6,.L86
	stw 28,0(18)
.L86:
	lwz 9,8(27)
	cmpwi 1,9,0
	bc 12,6,.L87
	lwz 0,12(27)
	stw 0,0(9)
	li 3,0
	b .L108
.L87:
	lwz 3,12(27)
	b .L108
.L72:
	cmpwi 1,19,0
	bc 12,6,.L89
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwzx 9,26,0
	lbz 0,1(9)
	cmpwi 1,0,45
	bc 12,6,.L89
	addis 9,0,nextchar@ha
	lwz 9,nextchar@l(9)
	mr 3,22
	lbz 4,0(9)
	bl my_index
	cmpwi 1,3,0
	bc 4,6,.L55
.L89:
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L90
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwzx 9,26,0
	lbz 0,1(9)
	cmpwi 1,0,45
	bc 4,6,.L91
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	addis 11,0,nextchar@ha
	lwz 3,12(9)
	addis 4,0,.LC6@ha
	addi 4,4,.LC6@l
	lwz 5,0(26)
	lwz 6,nextchar@l(11)
	crxor 6,6,6
	bl fprintf
	b .L90
.L91:
	addis 9,0,_impure_ptr@ha
	lwz 11,_impure_ptr@l(9)
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	slwi 0,0,2
	lwzx 9,26,0
	addis 10,0,nextchar@ha
	lwz 3,12(11)
	addis 4,0,.LC7@ha
	addi 4,4,.LC7@l
	lwz 5,0(26)
	lbz 6,0(9)
	lwz 7,nextchar@l(10)
	crxor 6,6,6
	bl fprintf
.L90:
	addis 11,0,nextchar@ha
	addis 9,0,.LC8@ha
	addi 9,9,.LC8@l
	stw 9,nextchar@l(11)
.L116:
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	addic 0,0,1
	stw 0,optind@l(9)
	li 3,0
	b .L108
.L55:
	addis 29,0,nextchar@ha
	lwz 9,nextchar@l(29)
	lbz 31,0(9)
	addi 9,9,1
	stw 9,nextchar@l(29)
	mr 3,22
	mr 4,31
	bl my_index
	lwz 9,nextchar@l(29)
	lbz 0,0(9)
	cmpwi 1,0,0
	bc 4,6,.L93
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	addic 0,0,1
	stw 0,optind@l(9)
.L93:
	subfic 0,3,0
	adde 9,0,3
	xori 0,31,58
	subfic 11,0,0
	adde 0,11,0
	or. 11,9,0
	bc 12,2,.L94
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L95
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC9@ha
	addi 4,4,.LC9@l
	lwz 5,0(26)
	mr 6,31
	crxor 6,6,6
	bl fprintf
.L95:
	addis 9,0,optopt@ha
	stw 31,optopt@l(9)
	li 3,0
	b .L108
.L94:
	lbz 0,1(3)
	cmpwi 1,0,58
	bc 4,6,.L96
	lbz 0,2(3)
	cmpwi 1,0,58
	bc 4,6,.L97
	addis 9,0,nextchar@ha
	lwz 11,nextchar@l(9)
	lbz 0,0(11)
	cmpwi 1,0,0
	bc 4,6,.L117
	addis 9,0,optarg@ha
	li 0,0
	stw 0,optarg@l(9)
	b .L102
.L97:
	addis 9,0,nextchar@ha
	lwz 11,nextchar@l(9)
	lbz 0,0(11)
	cmpwi 1,0,0
	bc 12,6,.L101
.L117:
	addis 9,0,optarg@ha
	stw 11,optarg@l(9)
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	addic 0,0,1
	stw 0,optind@l(9)
	b .L102
.L101:
	addis 9,0,optind@ha
	lwz 0,optind@l(9)
	cmpw 1,0,21
	bc 4,6,.L103
	addis 9,0,opterr@ha
	lwz 0,opterr@l(9)
	cmpwi 1,0,0
	bc 12,6,.L104
	addis 9,0,_impure_ptr@ha
	lwz 9,_impure_ptr@l(9)
	lwz 3,12(9)
	addis 4,0,.LC10@ha
	addi 4,4,.LC10@l
	lwz 5,0(26)
	mr 6,31
	crxor 6,6,6
	bl fprintf
.L104:
	addis 9,0,optopt@ha
	stw 31,optopt@l(9)
	lbz 0,0(22)
	xori 0,0,58
	addic 0,0,-1
	subfe 0,0,0
	andi. 31,0,58
	b .L102
.L103:
	addis 10,0,optarg@ha
	addis 11,0,optind@ha
	lwz 0,optind@l(11)
	slwi 9,0,2
	lwzx 9,9,26
	stw 9,optarg@l(10)
	addic 0,0,1
	stw 0,optind@l(11)
.L102:
	addis 9,0,nextchar@ha
	li 0,0
	stw 0,nextchar@l(9)
.L96:
	mr 3,31
.L108:
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
.Lfe4:
	.size	 _getopt_internal,.Lfe4-_getopt_internal
	.align 2
	.globl getopt
	.type	 getopt,@function
getopt:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	li 6,0
	mr 7,6
	mr 8,6
	bl _getopt_internal
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe5:
	.size	 getopt,.Lfe5-getopt
	.align 2
	.globl getopt_long
	.type	 getopt_long,@function
getopt_long:
	stwu 1,-8(1)
	mflr 0
	stw 0,12(1)
	li 8,0
	bl _getopt_internal
	lwz 0,12(1)
	mtlr 0
	addi 1,1,8
	blr
.Lfe6:
	.size	 getopt_long,.Lfe6-getopt_long
	.section ".sdata","aw"
	.align 2
nextchar:
	.space	4
	.size	 nextchar,4
	.align 2
ordering:
	.space	4
	.size	 ordering,4
	.align 2
first_nonopt:
	.space	4
	.size	 first_nonopt,4
	.align 2
last_nonopt:
	.space	4
	.size	 last_nonopt,4
	.ident	"GCC: (GNU) 2.7-97r2"
