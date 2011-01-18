	.file	"values.adb"

 # rs6000/powerpc options: -msdata=data -G 8
	.gnu_attribute 4, 1
	.gnu_attribute 8, 1
	.gnu_attribute 12, 1
 # GNU Ada (GCC) version 4.5.3 20110115 for GNAT Pro 6.5.0w (20110114) (powerpc-elf)
 #	compiled by GNU C version 4.5.3 20110115 for GNAT Pro 6.5.0w (20110114), GMP version 4.2.2, MPFR version 2.3.1, MPC version 0.8
 # GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
 # options passed:  -fpreserve-control-flow -fverbose-asm -g values.adb
 # options enabled:  -falign-loops -fargument-alias -fauto-inc-dec
 # -fbranch-count-reg -fcommon -fdelete-null-pointer-checks
 # -fdwarf2-cfi-asm -fearly-inlining -ffunction-cse -fgcse-lm -fident
 # -fira-share-save-slots -fira-share-spill-slots -fivopts
 # -fkeep-static-consts -fleading-underscore -fmath-errno
 # -fmerge-debug-strings -fmove-loop-invariants -fpeephole
 # -fpreserve-control-flow -freg-struct-return
 # -fsched-critical-path-heuristic -fsched-dep-count-heuristic
 # -fsched-group-heuristic -fsched-interblock -fsched-last-insn-heuristic
 # -fsched-rank-heuristic -fsched-spec -fsched-spec-insn-heuristic
 # -fsched-stalled-insns-dep -fshow-column -fsigned-zeros
 # -fsplit-ivs-in-unroller -ftrapping-math -ftree-cselim -ftree-forwprop
 # -ftree-loop-im -ftree-loop-ivcanon -ftree-loop-optimize
 # -ftree-parallelize-loops= -ftree-phiprop -ftree-pta -ftree-reassoc
 # -ftree-scev-cprop -ftree-slp-vectorize -ftree-vect-loop-version
 # -funit-at-a-time -fverbose-asm -m32 -malign-branch-targets -malways-hint
 # -mbig -mbig-endian -mbss-plt -mfp-in-toc -mfused-madd
 # -mgen-cell-microcode -mhard-float -mnew-mnemonics -mpowerpc
 # -msched-groups -msched-prolog -mstrict-align -msvr4-struct-return
 # -mtls-markers -mupdate -mvectorize-builtins -mvsx-scalar-double

	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	".text"
.Ltext0:
	.cfi_sections	.debug_frame
	.globl values_E
	.section	.sdata,"aw",@progbits
	.type	values_E, @object
	.size	values_E, 1
values_E:
	.zero	1
	.section	".text"
	.align 2
	.globl values__intIP
	.type	values__intIP, @function
values__intIP:
.LFB2:
	.cfi_startproc
	stwu 1,-32(1)	 #,,
.LCFI0:
	.cfi_def_cfa_offset 32
	stw 31,28(1)	 #,
	mr 31,1	 #,
	.cfi_offset 31, -4
.LCFI1:
	.cfi_def_cfa_register 31
	stw 3,8(31)	 # _init, _init
	lwz 0,8(31)	 # _init, tmp119
	li 9,0	 # tmp120,
	mr 11,0	 #, tmp119
	stw 9,0(11)	 # _init_1(D)->val, tmp120
	lwz 0,8(31)	 # _init, tmp121
	li 9,0	 # tmp122,
	mr 11,0	 #, tmp121
	stb 9,4(11)	 # _init_1(D)->set, tmp122
	addi 11,31,32	 #,,
	lwz 31,-4(11)	 #,
.LCFI2:
	.cfi_def_cfa 11, 0
	mr 1,11	 #,
.LCFI3:
	.cfi_def_cfa_register 1
	.cfi_restore 31
	blr	 #
	.cfi_endproc
.LFE2:
	.size	values__intIP, .-values__intIP
	.section	.rodata
	.align 2
.LC1:
	.ascii	"failed precondition from values.ads:9"
	.align 2
.LC0:
 # LB0:
	.long	1
 # UB0:
	.long	37
	.section	".text"
	.align 2
	.globl values__set
	.type	values__set, @function
values__set:
.LFB3:
	.file 1 "values.adb"
	.loc 1 4 0
	.cfi_startproc
	stwu 1,-48(1)	 #,,
.LCFI4:
	.cfi_def_cfa_offset 48
	mflr 0	 #,
	stw 0,52(1)	 #,
	stw 31,44(1)	 #,
	mr 31,1	 #,
	.cfi_offset 31, -4
	.cfi_offset 65, 4
.LCFI5:
	.cfi_def_cfa_register 31
	stw 3,24(31)	 # v, v
	stw 4,28(31)	 # val, val
	mr 0,5	 # tmp121, reset_ok
	stb 0,32(31)	 # reset_ok, tmp121
	.file 2 "values.ads"
	.loc 2 9 0
	lwz 0,24(31)	 # v, tmp122
	mr 9,0	 #, tmp122
	lbz 0,4(9)	 # v_1(D)->set, tmp123
	rlwinm 0,0,0,0xff	 # D.1342, tmp123
	cmpwi 7,0,0	 #, tmp124, D.1342
	.loc 2 9 37
	beq 7,.L4	 #
	.loc 2 9 37 is_stmt 0 discriminator 1
	lbz 0,32(31)	 # reset_ok, tmp126
	rlwinm 0,0,0,0xff	 # tmp125, tmp126
	cmpwi 7,0,0	 #, tmp127, tmp125
	.loc 2 9 45 is_stmt 1 discriminator 1
	bne 7,.L4	 #
.LBB2:
	lis 0,.LC1@ha	 # tmp128,
	addic 0,0,.LC1@l	 # D.1348, tmp128,
	stw 0,8(31)	 # C.6.P_ARRAY, D.1348
	lis 0,.LC0@ha	 # tmp130,
	addic 0,0,.LC0@l	 # tmp129, tmp130,
	stw 0,12(31)	 # C.6.P_BOUNDS, tmp129
	addi 0,31,8	 # tmp135,,
	mr 3,0	 #, tmp135
	bl system__assertions__raise_assert_failure	 #
.L4:
.LBE2:
	.loc 1 6 0
	lwz 0,24(31)	 # v, tmp131
	lwz 9,28(31)	 # val, tmp132
	mr 11,0	 #, tmp131
	stw 9,0(11)	 # v_1(D)->val, tmp132
	lwz 0,24(31)	 # v, tmp133
	li 9,1	 # tmp134,
	mr 11,0	 #, tmp133
	stb 9,4(11)	 # v_1(D)->set, tmp134
	.loc 1 7 0
	nop
	addi 11,31,48	 #,,
	lwz 0,4(11)	 #,
	mtlr 0	 #,
	lwz 31,-4(11)	 #,
.LCFI6:
	.cfi_def_cfa 11, 0
	mr 1,11	 #,
.LCFI7:
	.cfi_def_cfa_register 1
	.cfi_restore 31
	.loc 1 7 7
	blr	 #
	.cfi_endproc
.LFE3:
	.size	values__set, .-values__set
.Letext0:
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.4byte	.LFB3-.Ltext0
	.4byte	.LCFI4-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 0
	.4byte	.LCFI4-.Ltext0
	.4byte	.LCFI5-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 48
	.4byte	.LCFI5-.Ltext0
	.4byte	.LCFI6-.Ltext0
	.2byte	0x2
	.byte	0x8f
	.sleb128 48
	.4byte	.LCFI6-.Ltext0
	.4byte	.LCFI7-.Ltext0
	.2byte	0x2
	.byte	0x7b
	.sleb128 0
	.4byte	.LCFI7-.Ltext0
	.4byte	.LFE3-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 0
	.4byte	0x0
	.4byte	0x0
	.section	.debug_info
	.4byte	0xc4
	.2byte	0x2
	.4byte	.Ldebug_abbrev0
	.byte	0x4
	.uleb128 0x1
	.4byte	.LASF1
	.byte	0xd
	.4byte	.LASF2
	.4byte	.LASF3
	.4byte	.Ltext0
	.4byte	.Letext0
	.4byte	.Ldebug_line0
	.uleb128 0x2
	.byte	0x1
	.4byte	.LASF4
	.byte	0x1
	.byte	0x4
	.4byte	.LFB3
	.4byte	.LFE3
	.4byte	.LLST0
	.4byte	0x6f
	.uleb128 0x3
	.string	"v"
	.byte	0x2
	.byte	0x8
	.4byte	0xa7
	.byte	0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x3
	.string	"val"
	.byte	0x2
	.byte	0x8
	.4byte	0xb2
	.byte	0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0x4
	.4byte	.LASF0
	.byte	0x2
	.byte	0x8
	.4byte	0xb7
	.byte	0x2
	.byte	0x91
	.sleb128 -16
	.uleb128 0x5
	.4byte	.LBB2
	.4byte	.LBE2
	.byte	0x0
	.uleb128 0x6
	.4byte	.LASF5
	.byte	0x8
	.byte	0x2
	.byte	0x3
	.4byte	0x98
	.uleb128 0x7
	.string	"val"
	.byte	0x2
	.byte	0x4
	.4byte	0x98
	.byte	0x2
	.byte	0x23
	.uleb128 0x0
	.uleb128 0x7
	.string	"set"
	.byte	0x2
	.byte	0x5
	.4byte	0xa0
	.byte	0x2
	.byte	0x23
	.uleb128 0x4
	.byte	0x0
	.uleb128 0x8
	.byte	0x4
	.byte	0x5
	.4byte	.LASF6
	.byte	0x1
	.uleb128 0x9
	.byte	0x1
	.byte	0x2
	.4byte	.LASF7
	.uleb128 0xa
	.4byte	0xac
	.uleb128 0xb
	.byte	0x4
	.4byte	0x6f
	.uleb128 0xa
	.4byte	0x98
	.uleb128 0xa
	.4byte	0xa0
	.uleb128 0xc
	.4byte	.LASF5
	.byte	0x2
	.byte	0x3
	.4byte	0x6f
	.byte	0x0
	.section	.debug_abbrev
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x10
	.uleb128 0x6
	.byte	0x0
	.byte	0x0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0x6
	.uleb128 0x1
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x5
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x4
	.uleb128 0x5
	.byte	0x0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x5
	.uleb128 0xb
	.byte	0x0
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.byte	0x0
	.byte	0x0
	.uleb128 0x6
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0x7
	.uleb128 0xd
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x8
	.uleb128 0x24
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x34
	.uleb128 0xc
	.byte	0x0
	.byte	0x0
	.uleb128 0x9
	.uleb128 0x24
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0x0
	.byte	0x0
	.uleb128 0xa
	.uleb128 0x26
	.byte	0x0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0xb
	.uleb128 0x10
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0xc
	.uleb128 0x16
	.byte	0x0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.byte	0x0
	.section	.debug_aranges,"",@progbits
	.4byte	0x1c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x4
	.byte	0x0
	.2byte	0x0
	.2byte	0x0
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	0x0
	.4byte	0x0
	.section	.debug_str,"MS",@progbits,1
.LASF4:
	.string	"values__set"
.LASF0:
	.string	"reset_ok"
.LASF3:
	.string	"/home/hainque/couverture/testsuite/Qualif/Ada/mcdc/1_Core/ShapesAndContexts/Preconditions/OrInSpec/src"
.LASF5:
	.string	"values__int"
.LASF6:
	.string	"integer"
.LASF2:
	.string	"values.adb"
.LASF1:
	.string	"GNU Ada 4.5.3 20110115 for GNAT Pro 6.5.0w (20110114)"
.LASF7:
	.string	"boolean"
	.ident	"GCC: (GNU) 4.5.3 20110115 for GNAT Pro 6.5.0w (20110114)"
