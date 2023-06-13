	.text

	.type f, %function
f:
    save %sp, -96, %sp
	add %g0, %g0, %g0
	b local
local:
	restore
    jmp %o7+8
	.size f, .-f

	.type foo, %function
foo:
	mov %g0, %i0
	bl f
	.size foo, .-foo
