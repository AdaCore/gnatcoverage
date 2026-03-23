	.section	".text_cond"
	.align 2
	.globl cond
	.type	cond, @function
cond:
	cmpwi %cr7,%r3,%r0
        li    %r3,120
        beqlr   %cr7
        # != 0
        li    %r3,2
        blr
	.size	cond, .-cond
