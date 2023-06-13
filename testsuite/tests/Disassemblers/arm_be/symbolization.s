	.text
	.align 0

	.type f, %function
f:
	add r0, r0, r0
	b local
local:
	mov pc, lr
	.size f, .-f

	.type foo, %function
foo:
	mov r0, #1
	bl f
	.size foo, .-foo
