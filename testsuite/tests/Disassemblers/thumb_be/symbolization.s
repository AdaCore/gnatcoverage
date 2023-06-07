	.text
	.arch armv4t
	.syntax unified
	.thumb
	.align 0

	.type f, %function
f:
	adds r0, #3
	b local
local:
	mov pc, lr
	.size f, .-f

	.type foo, %function
foo:
	movs r0, #1
	bl f
	.size foo, .-foo
