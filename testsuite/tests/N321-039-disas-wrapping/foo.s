.cpu cortex-m4
.arch armv7e-m
.fpu fpv4-sp-d16
.file "foo.c"
.text
.align 2
.syntax unified
.thumb
.thumb_func
.type _start, %function
_start:
mov r0, r1
mov r0, r1
.size _start, .-_start
