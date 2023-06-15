	.text

	.type f, %function
f:
	add.l r0,r0,r0
	bra tr,r0,r0
	nop
local:
	bra tr,r21,r0
	nop
	.size f, .-f

	.type foo, %function
foo:
	moviu r10,%u f
	moviu r10,%l f
	bra tr,r10,r21
	nop
	.size foo, .-foo
