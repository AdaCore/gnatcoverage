# With
# - empty symbols at the beginning,
# - empty symbol and orphan region in the middle,
# - empty symbol at the end
        
	.file	"t.c"
	.section	".text"
        #
	.global empty_sym_at_start
	.type	empty_sym_at_start, @function
empty_sym_at_start:
	.size	empty_sym_at_start, 0
        #        
	.globl plop
	.type	plop, @function
plop:
	blr
	.size	plop, .-plop
        #
	.global empty_sym_in_between
	.type	empty_sym_in_between, @function
empty_sym_in_between:
	.size	empty_sym_in_between, 0
        #
        .skip 1024
        #        
	.globl plop1
	.type	plop1, @function
plop1:
	blr
	.size	plop1, .-plop1
        #
	.global empty_sym_at_end
	.type	empty_sym_at_end, @function
empty_sym_at_end:
	.size	empty_sym_at_end, 0
                
