	.text

	.type f, %function
f:
	b local
	nop
local:
	blr
	.size f, .-f

	.type foo, %function
foo:
	bl f
	nop
	.size foo, .-foo
