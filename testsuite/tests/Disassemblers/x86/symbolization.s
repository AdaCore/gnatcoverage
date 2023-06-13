	.text

	.type f, %function
f:
	add $2, %eax
	jmp local
	nop
local:
	ret
	.size f, .-f

	.type foo, %function
foo:
	mov $1, %eax
	call f
	ret
	.size foo, .-foo
