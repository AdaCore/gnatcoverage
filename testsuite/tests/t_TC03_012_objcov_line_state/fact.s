	.arch armv8-a+crc
	.file	"fact.c"
	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.align	2
	.global	fact
	.type	fact, %function
fact:
.LFB0:
	.file 1 "fact.c"
	.loc 1 3 1
	.cfi_startproc
	stp	x29, x30, [sp, -32]!
	.cfi_def_cfa_offset 32
	.cfi_offset 29, -32
	.cfi_offset 30, -24
	mov	x29, sp
	str	w0, [sp, 28]
	.loc 1 4 9
	ldr	w0, [sp, 28]
	cmp	w0, 0
	bne	.L2
# The following instruction (mov w0, 1) was manually inserted so that we have
# an instruction that is assigned the same sloc as the conditional branch above
# (bne, line 4), but that will not be executed even though the branch is
# executed. Because of this, the coverage report is expected to report line 4
# as partially covered (it used to wrongly report it as fully covered).
	mov	w0, 1
	.loc 1 5 12
	mov	w0, 1
	b	.L3
.L2:
	.loc 1 6 14
	ldr	w0, [sp, 28]
	cmp	w0, 1
	bgt	.L4
	.loc 1 7 12
	mov	w0, 1
	b	.L3
.L4:
	.loc 1 9 16
	ldr	w0, [sp, 28]
	sub	w0, w0, #1
	bl	fact
	mov	w1, w0
	.loc 1 9 14 discriminator 1
	ldr	w0, [sp, 28]
	mul	w0, w1, w0
.L3:
	.loc 1 10 1
	nop
	ldp	x29, x30, [sp], 32
	.cfi_restore 30
	.cfi_restore 29
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE0:
	.size	fact, .-fact
.Letext0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0x62
	.2byte	0x5
	.byte	0x1
	.byte	0x8
	.4byte	.Ldebug_abbrev0
	.uleb128 0x1
	.4byte	.LASF2
	.byte	0x1d
	.4byte	.LASF0
	.4byte	.LASF1
	.8byte	.Ltext0
	.8byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.uleb128 0x2
	.4byte	.LASF3
	.byte	0x1
	.byte	0x2
	.byte	0x1
	.4byte	0x5e
	.8byte	.LFB0
	.8byte	.LFE0-.LFB0
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x5e
	.uleb128 0x3
	.string	"n"
	.byte	0x1
	.byte	0x2
	.byte	0xb
	.4byte	0x5e
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x1f
	.uleb128 0x1b
	.uleb128 0x1f
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x7c
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.4byte	0x2c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x8
	.byte	0
	.2byte	0
	.2byte	0
	.8byte	.Ltext0
	.8byte	.Letext0-.Ltext0
	.8byte	0
	.8byte	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF3:
	.string	"fact"
.LASF2:
	.string	"GNU C17 14.3.1 20250720 (for GNAT Pro 26.0w 20250720) -mcpu=cortex-a53 -mlittle-endian -mabi=lp64 -g -fno-tree-loop-distribute-patterns -fpreserve-control-flow -fpreserve-decisions-generic"
	.section	.debug_line_str,"MS",@progbits,1
.LASF0:
	.string	"fact.c"
.LASF1:
	.string	".."
	.ident	"GCC: (GNU) 14.3.1 20250720 (for GNAT Pro 26.0w 20250720)"
