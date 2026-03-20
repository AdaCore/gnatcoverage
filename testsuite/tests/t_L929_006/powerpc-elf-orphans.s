# With
# - orphan region at the beginning,
# - empty symbol and orphan region in the middle,
# - empty last symbol, then orphan region at the end
        
	.section	".text"
        #
        .skip 1024
	.align 4
        #
	.global plop
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
	.global plop1
	.type	plop1, @function
plop1:
	blr
	.size	plop1, .-plop1
        #
	.global empty_last_sym
	.type	empty_last_sym, @function
empty_last_sym:
	.size	empty_last_sym, 0
        #
        .skip 1024
        