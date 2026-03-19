	.file	"min.adb"
	.gnu_attribute 4, 1
	.gnu_attribute 8, 1
	.section	".text"
	.align 2
	.globl min
	.type	min, @function
min:
	mr 0,3
	mr 3,4
#	cmpw 7,4,0
#	blelr 7
	cmpw 7,0,4
	bgtlr 7
	mr 3,0
	blr
	.size	min, .-min
