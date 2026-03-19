	.text
	.globl func
	.type func,@function
func:
	shld %cl,%eax,%edx
	.size func, . - func
