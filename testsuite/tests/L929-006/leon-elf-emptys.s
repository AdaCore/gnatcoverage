# With
# - empty symbols at the beginning,
# - empty symbol and orphan region in the middle,
# - empty symbol at the end
        
	.section	".text"
        #
	.global empty_sym_at_start
	.type	empty_sym_at_start, #function
empty_sym_at_start:
	.size	empty_sym_at_start, 0
        #
	.global plop
	.type	plop, #function
	.proc	020
plop:
	save	%sp, -96, %sp
	restore
	jmp	%o7+8
	 nop
	.size	plop, .-plop
        #
	.global empty_sym_in_between
	.type	empty_sym_in_between, #function
empty_sym_in_between:
	.size	empty_sym_in_between, 0
        #
        .skip 1024
        #
	.global plop1
	.type	plop1, #function
	.proc	020
plop1:
	save	%sp, -96, %sp
	restore
	jmp	%o7+8
	 nop
	.size	plop1, .-plop1
        #
	.global empty_sym_at_end
	.type	empty_sym_at_end, #function
empty_sym_at_end:
	.size	empty_sym_at_end, 0
        