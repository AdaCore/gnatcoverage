.cpu cortex-m4
.arch armv7e-m
.fpu fpv4-sp-d16
.file "foo.c"
.text
.align 2
.type _start, %function
_start:
.word 0
.size _start, .-_start
