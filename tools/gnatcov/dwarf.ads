------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2021, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Dwarf is
   --  DWARF encodings for tags, attributes, forms and operations, as defined
   --  in section 7 (data representation) of the DWARF 3 standard (and in
   --  particular figures 18, 19, 20, 24, 25, 28, 31, 32, 33, 34, 38, 40).

   DW_TAG_array_type               : constant := 16#01#;
   DW_TAG_class_type               : constant := 16#02#;
   DW_TAG_entry_point              : constant := 16#03#;
   DW_TAG_enumeration_type         : constant := 16#04#;
   DW_TAG_formal_parameter         : constant := 16#05#;
   DW_TAG_imported_declaration     : constant := 16#08#;
   DW_TAG_label                    : constant := 16#0a#;
   DW_TAG_lexical_block            : constant := 16#0b#;
   DW_TAG_member                   : constant := 16#0d#;
   DW_TAG_pointer_type             : constant := 16#0f#;
   DW_TAG_reference_type           : constant := 16#10#;
   DW_TAG_compile_unit             : constant := 16#11#;
   DW_TAG_string_type              : constant := 16#12#;
   DW_TAG_structure_type           : constant := 16#13#;
   DW_TAG_subroutine_type          : constant := 16#15#;
   DW_TAG_typedef                  : constant := 16#16#;
   DW_TAG_union_type               : constant := 16#17#;
   DW_TAG_unspecified_parameters   : constant := 16#18#;
   DW_TAG_variant                  : constant := 16#19#;
   DW_TAG_common_block             : constant := 16#1a#;
   DW_TAG_common_inclusion         : constant := 16#1b#;
   DW_TAG_inheritance              : constant := 16#1c#;
   DW_TAG_inlined_subroutine       : constant := 16#1d#;
   DW_TAG_module                   : constant := 16#1e#;
   DW_TAG_ptr_to_member_type       : constant := 16#1f#;
   DW_TAG_set_type                 : constant := 16#20#;
   DW_TAG_subrange_type            : constant := 16#21#;
   DW_TAG_with_stmt                : constant := 16#22#;
   DW_TAG_access_declaration       : constant := 16#23#;
   DW_TAG_base_type                : constant := 16#24#;
   DW_TAG_catch_block              : constant := 16#25#;
   DW_TAG_const_type               : constant := 16#26#;
   DW_TAG_constant                 : constant := 16#27#;
   DW_TAG_enumerator               : constant := 16#28#;
   DW_TAG_file_type                : constant := 16#29#;
   DW_TAG_friend                   : constant := 16#2a#;
   DW_TAG_namelist                 : constant := 16#2b#;
   DW_TAG_namelist_item            : constant := 16#2c#;
   DW_TAG_packed_type              : constant := 16#2d#;
   DW_TAG_subprogram               : constant := 16#2e#;
   DW_TAG_template_type_parameter  : constant := 16#2f#;
   DW_TAG_template_value_parameter : constant := 16#30#;
   DW_TAG_thrown_type              : constant := 16#31#;
   DW_TAG_try_block                : constant := 16#32#;
   DW_TAG_variant_part             : constant := 16#33#;
   DW_TAG_variable                 : constant := 16#34#;
   DW_TAG_volatile_type            : constant := 16#35#;
   DW_TAG_dwarf_procedure          : constant := 16#36#;
   DW_TAG_restrict_type            : constant := 16#37#;
   DW_TAG_interface_type           : constant := 16#38#;
   DW_TAG_namespace                : constant := 16#39#;
   DW_TAG_imported_module          : constant := 16#3a#;
   DW_TAG_unspecified_type         : constant := 16#3b#;
   DW_TAG_partial_unit             : constant := 16#3c#;
   DW_TAG_imported_unit            : constant := 16#3d#;
   DW_TAG_mutable_type             : constant := 16#3e#;
   DW_TAG_lo_user                  : constant := 16#4080#;
   DW_TAG_GNU_call_site            : constant := 16#4109#;
   DW_TAG_hi_user                  : constant := 16#Ffff#;

   DW_CHILDREN_no      : constant := 16#0#;
   DW_CHILDREN_yes     : constant := 16#1#;

   DW_AT_sibling              : constant := 16#01#; -- reference
   DW_AT_location             : constant := 16#02#; -- block, loclistptr
   DW_AT_name                 : constant := 16#03#; -- string
   DW_AT_ordering             : constant := 16#09#; -- constant
   DW_AT_byte_size            : constant := 16#0b#; -- block, constant, ref
   DW_AT_bit_offset           : constant := 16#0c#; -- block, constant, ref
   DW_AT_bit_size             : constant := 16#0d#; -- block, constant, ref
   DW_AT_stmt_list            : constant := 16#10#; -- lineptr
   DW_AT_low_pc               : constant := 16#11#; -- address
   DW_AT_high_pc              : constant := 16#12#; -- address
   DW_AT_language             : constant := 16#13#; -- constant
   DW_AT_discr                : constant := 16#15#; -- reference
   DW_AT_discr_value          : constant := 16#16#; -- constant
   DW_AT_visibility           : constant := 16#17#; -- constant
   DW_AT_import               : constant := 16#18#; -- reference
   DW_AT_string_length        : constant := 16#19#; -- block, loclistptr
   DW_AT_common_reference     : constant := 16#1a#; -- reference
   DW_AT_comp_dir             : constant := 16#1b#; -- string
   DW_AT_const_value          : constant := 16#1c#; -- block, constant, string
   DW_AT_containing_type      : constant := 16#1d#; -- reference
   DW_AT_default_value        : constant := 16#1e#; -- reference
   DW_AT_inline               : constant := 16#20#; -- constant
   DW_AT_is_optional          : constant := 16#21#; -- flag
   DW_AT_lower_bound          : constant := 16#22#; -- block, constant, ref
   DW_AT_producer             : constant := 16#25#; -- string
   DW_AT_prototyped           : constant := 16#27#; -- flag
   DW_AT_return_addr          : constant := 16#2a#; -- block, loclistptr
   DW_AT_start_scope          : constant := 16#2c#; -- constant
   DW_AT_stride_size          : constant := 16#2e#; -- constant
   DW_AT_upper_bound          : constant := 16#2f#; -- block, constant, ref
   DW_AT_abstract_origin      : constant := 16#31#; -- reference
   DW_AT_accessibility        : constant := 16#32#; -- constant
   DW_AT_address_class        : constant := 16#33#; -- constant
   DW_AT_artificial           : constant := 16#34#; -- flag
   DW_AT_base_types           : constant := 16#35#; -- reference
   DW_AT_calling_convention   : constant := 16#36#; -- constant
   DW_AT_count                : constant := 16#37#; -- block, constant, ref
   DW_AT_data_member_location : constant := 16#38#; -- block, const, loclistptr
   DW_AT_decl_column          : constant := 16#39#; -- constant
   DW_AT_decl_file            : constant := 16#3a#; -- constant
   DW_AT_decl_line            : constant := 16#3b#; -- constant
   DW_AT_declaration          : constant := 16#3c#; -- flag
   DW_AT_discr_list           : constant := 16#3d#; -- block
   DW_AT_encoding             : constant := 16#3e#; -- constant
   DW_AT_external             : constant := 16#3f#; -- flag
   DW_AT_frame_base           : constant := 16#40#; -- block, loclistptr
   DW_AT_friend               : constant := 16#41#; -- reference
   DW_AT_identifier_case      : constant := 16#42#; -- constant
   DW_AT_macro_info           : constant := 16#43#; -- macptr
   DW_AT_namelist_item        : constant := 16#44#; -- block
   DW_AT_priority             : constant := 16#45#; -- reference
   DW_AT_segment              : constant := 16#46#; -- block, constant
   DW_AT_specification        : constant := 16#47#; -- reference
   DW_AT_static_link          : constant := 16#48#; -- block, loclistptr
   DW_AT_type                 : constant := 16#49#; -- reference
   DW_AT_use_location         : constant := 16#4a#; -- block, loclistptr
   DW_AT_variable_parameter   : constant := 16#4b#; -- flag
   DW_AT_virtuality           : constant := 16#4c#; -- constant
   DW_AT_vtable_elem_location : constant := 16#4d#; -- block, loclistptr
   DW_AT_allocated            : constant := 16#4e#; -- block, constant, ref
   DW_AT_associated           : constant := 16#4f#; -- block, constant, ref
   DW_AT_data_location        : constant := 16#50#; -- x50block
   DW_AT_stride               : constant := 16#51#; -- block, constant, ref
   DW_AT_entry_pc             : constant := 16#52#; -- address
   DW_AT_use_UTF8             : constant := 16#53#; -- flag
   DW_AT_extension            : constant := 16#04#; -- reference
   DW_AT_ranges               : constant := 16#55#; -- rangelistptr
   DW_AT_trampoline           : constant := 16#56#; -- address, flag, ref, str
   DW_AT_call_column          : constant := 16#57#; -- constant
   DW_AT_call_file            : constant := 16#58#; -- constant
   DW_AT_call_line            : constant := 16#59#; -- constant
   DW_AT_description          : constant := 16#5a#; -- string
   DW_AT_linkage_name         : constant := 16#6e#; -- string
   DW_AT_lo_user              : constant := 16#2000#; -- ---
   DW_AT_MIPS_linkage_name    : constant := 16#2007#; -- string
   DW_AT_hi_user              : constant := 16#3fff#; -- ---

   DW_FORM_addr         : constant := 16#01#; -- address
   DW_FORM_block2       : constant := 16#03#; -- block
   DW_FORM_block4       : constant := 16#04#; -- block
   DW_FORM_data2        : constant := 16#05#; -- constant
   DW_FORM_data4        : constant := 16#06#;
   --  constant, lineptr, loclistptr, macptr, rangelistptr
   DW_FORM_data8        : constant := 16#07#; -- Likewise
   DW_FORM_string       : constant := 16#08#; -- string
   DW_FORM_block        : constant := 16#09#; -- block
   DW_FORM_block1       : constant := 16#0a#; -- block
   DW_FORM_data1        : constant := 16#0b#; -- constant
   DW_FORM_flag         : constant := 16#0c#; -- flag
   DW_FORM_sdata        : constant := 16#0d#; -- constant
   DW_FORM_strp         : constant := 16#0e#; -- string
   DW_FORM_udata        : constant := 16#0f#; -- constant
   DW_FORM_ref_addr     : constant := 16#10#; -- reference
   DW_FORM_ref1         : constant := 16#11#; -- reference
   DW_FORM_ref2         : constant := 16#12#; -- reference
   DW_FORM_ref4         : constant := 16#13#; -- reference
   DW_FORM_ref8         : constant := 16#14#; -- reference
   DW_FORM_ref_udata    : constant := 16#15#; -- reference
   DW_FORM_indirect     : constant := 16#16#; -- (see Section 7.5.3)
   DW_FORM_sec_offset   : constant := 16#17#;
   --  lineptr, loclistptr, macptr, rangelistptr
   DW_FORM_exprloc      : constant := 16#18#; -- exprloc
   DW_FORM_flag_present : constant := 16#19#; -- flag
   DW_FORM_ref_sig8     : constant := 16#20#; -- reference

   DW_OP_addr        : constant := 16#03#; -- 1 constant address (target spec)
   DW_OP_deref       : constant := 16#06#; -- 0
   DW_OP_const1u     : constant := 16#08#; -- 1 1-byte constant
   DW_OP_const1s     : constant := 16#09#; -- 1 1-byte constant
   DW_OP_const2u     : constant := 16#0a#; -- 1 2-byte constant
   DW_OP_const2s     : constant := 16#0b#; -- 1 2-byte constant
   DW_OP_const4u     : constant := 16#0c#; -- 1 4-byte constant
   DW_OP_const4s     : constant := 16#0d#; -- 1 4-byte constant
   DW_OP_const8u     : constant := 16#0e#; -- 1 8-byte constant
   DW_OP_const8s     : constant := 16#0f#; -- 1 8-byte constant
   DW_OP_constu      : constant := 16#10#; -- 1 ULEB128 constant
   DW_OP_consts      : constant := 16#11#; -- 1 SLEB128 constant
   DW_OP_dup         : constant := 16#12#; -- 0
   DW_OP_drop        : constant := 16#13#; -- 0
   DW_OP_over        : constant := 16#14#; -- 0
   DW_OP_pick        : constant := 16#15#; -- 1 1-byte stack index
   DW_OP_swap        : constant := 16#16#; -- 0
   DW_OP_rot         : constant := 16#17#; -- 0
   DW_OP_xderef      : constant := 16#18#; -- 0
   DW_OP_abs         : constant := 16#19#; -- 0
   DW_OP_and         : constant := 16#1a#; -- 0
   DW_OP_div         : constant := 16#1b#; -- 0
   DW_OP_minus       : constant := 16#1c#; -- 0
   DW_OP_mod         : constant := 16#1d#; -- 0
   DW_OP_mul         : constant := 16#1e#; -- 0
   DW_OP_neg         : constant := 16#1f#; -- 0
   DW_OP_not         : constant := 16#20#; -- 0
   DW_OP_or          : constant := 16#21#; -- 0
   DW_OP_plus        : constant := 16#22#; -- 0
   DW_OP_plus_uconst : constant := 16#23#; -- 1 ULEB128 addend
   DW_OP_shl         : constant := 16#24#; -- 0
   DW_OP_shr         : constant := 16#25#; -- 0
   DW_OP_shra        : constant := 16#26#; -- 0
   DW_OP_xor         : constant := 16#27#; -- 0
   DW_OP_skip        : constant := 16#2f#; -- 1 signed 2-byte constant
   DW_OP_bra         : constant := 16#28#; -- 1 signed 2-byte constant
   DW_OP_eq          : constant := 16#29#; -- 0
   DW_OP_ge          : constant := 16#2a#; -- 0
   DW_OP_gt          : constant := 16#2b#; -- 0
   DW_OP_le          : constant := 16#2c#; -- 0
   DW_OP_lt          : constant := 16#2d#; -- 0
   DW_OP_ne          : constant := 16#2e#; -- 0
   DW_OP_lit0        : constant := 16#30#; -- 0
   DW_OP_lit1        : constant := 16#31#; -- 0
   DW_OP_lit2        : constant := 16#32#; -- 0
   DW_OP_lit3        : constant := 16#33#; -- 0
   DW_OP_lit4        : constant := 16#34#; -- 0
   DW_OP_lit5        : constant := 16#35#; -- 0
   DW_OP_lit6        : constant := 16#36#; -- 0
   DW_OP_lit7        : constant := 16#37#; -- 0
   DW_OP_lit8        : constant := 16#38#; -- 0
   DW_OP_lit9        : constant := 16#39#; -- 0
   DW_OP_lit10       : constant := 16#3a#; -- 0
   DW_OP_lit11       : constant := 16#3b#; -- 0
   DW_OP_lit12       : constant := 16#3c#; -- 0
   DW_OP_lit13       : constant := 16#3d#; -- 0
   DW_OP_lit14       : constant := 16#3e#; -- 0
   DW_OP_lit15       : constant := 16#3f#; -- 0
   DW_OP_lit16       : constant := 16#40#; -- 0
   DW_OP_lit17       : constant := 16#41#; -- 0
   DW_OP_lit18       : constant := 16#42#; -- 0
   DW_OP_lit19       : constant := 16#43#; -- 0
   DW_OP_lit20       : constant := 16#44#; -- 0
   DW_OP_lit21       : constant := 16#45#; -- 0
   DW_OP_lit22       : constant := 16#46#; -- 0
   DW_OP_lit23       : constant := 16#47#; -- 0
   DW_OP_lit24       : constant := 16#48#; -- 0
   DW_OP_lit25       : constant := 16#49#; -- 0
   DW_OP_lit26       : constant := 16#4a#; -- 0
   DW_OP_lit27       : constant := 16#4b#; -- 0
   DW_OP_lit28       : constant := 16#4c#; -- 0
   DW_OP_lit29       : constant := 16#4d#; -- 0
   DW_OP_lit30       : constant := 16#4e#; -- 0
   DW_OP_lit31       : constant := 16#4f#; -- 0
   DW_OP_reg0        : constant := 16#50#; -- 0
   DW_OP_reg1        : constant := 16#51#; -- 0
   DW_OP_reg2        : constant := 16#52#; -- 0
   DW_OP_reg3        : constant := 16#53#; -- 0
   DW_OP_reg4        : constant := 16#54#; -- 0
   DW_OP_reg5        : constant := 16#55#; -- 0
   DW_OP_reg6        : constant := 16#56#; -- 0
   DW_OP_reg7        : constant := 16#57#; -- 0
   DW_OP_reg8        : constant := 16#58#; -- 0
   DW_OP_reg9        : constant := 16#59#; -- 0
   DW_OP_reg10       : constant := 16#5a#; -- 0
   DW_OP_reg11       : constant := 16#5b#; -- 0
   DW_OP_reg12       : constant := 16#5c#; -- 0
   DW_OP_reg13       : constant := 16#5d#; -- 0
   DW_OP_reg14       : constant := 16#5e#; -- 0
   DW_OP_reg15       : constant := 16#5f#; -- 0
   DW_OP_reg16       : constant := 16#60#; -- 0
   DW_OP_reg17       : constant := 16#61#; -- 0
   DW_OP_reg18       : constant := 16#62#; -- 0
   DW_OP_reg19       : constant := 16#63#; -- 0
   DW_OP_reg20       : constant := 16#64#; -- 0
   DW_OP_reg21       : constant := 16#65#; -- 0
   DW_OP_reg22       : constant := 16#66#; -- 0
   DW_OP_reg23       : constant := 16#67#; -- 0
   DW_OP_reg24       : constant := 16#68#; -- 0
   DW_OP_reg25       : constant := 16#69#; -- 0
   DW_OP_reg26       : constant := 16#6a#; -- 0
   DW_OP_reg27       : constant := 16#6b#; -- 0
   DW_OP_reg28       : constant := 16#6c#; -- 0
   DW_OP_reg29       : constant := 16#6d#; -- 0
   DW_OP_reg30       : constant := 16#6e#; -- 0
   DW_OP_reg31       : constant := 16#6f#; -- 0 reg 0..31
   DW_OP_breg0       : constant := 16#70#; -- 1 SLEB128 offset base reg
   DW_OP_breg1       : constant := 16#71#; -- 1 SLEB128 offset base reg
   DW_OP_breg2       : constant := 16#72#; -- 1 SLEB128 offset base reg
   DW_OP_breg3       : constant := 16#73#; -- 1 SLEB128 offset base reg
   DW_OP_breg4       : constant := 16#74#; -- 1 SLEB128 offset base reg
   DW_OP_breg5       : constant := 16#75#; -- 1 SLEB128 offset base reg
   DW_OP_breg6       : constant := 16#76#; -- 1 SLEB128 offset base reg
   DW_OP_breg7       : constant := 16#77#; -- 1 SLEB128 offset base reg
   DW_OP_breg8       : constant := 16#78#; -- 1 SLEB128 offset base reg
   DW_OP_breg9       : constant := 16#79#; -- 1 SLEB128 offset base reg
   DW_OP_breg10      : constant := 16#7a#; -- 1 SLEB128 offset base reg
   DW_OP_breg11      : constant := 16#7b#; -- 1 SLEB128 offset base reg
   DW_OP_breg12      : constant := 16#7c#; -- 1 SLEB128 offset base reg
   DW_OP_breg13      : constant := 16#7d#; -- 1 SLEB128 offset base reg
   DW_OP_breg14      : constant := 16#7e#; -- 1 SLEB128 offset base reg
   DW_OP_breg15      : constant := 16#7f#; -- 1 SLEB128 offset base reg
   DW_OP_breg16      : constant := 16#80#; -- 1 SLEB128 offset base reg
   DW_OP_breg17      : constant := 16#81#; -- 1 SLEB128 offset base reg
   DW_OP_breg18      : constant := 16#82#; -- 1 SLEB128 offset base reg
   DW_OP_breg19      : constant := 16#83#; -- 1 SLEB128 offset base reg
   DW_OP_breg20      : constant := 16#84#; -- 1 SLEB128 offset base reg
   DW_OP_breg21      : constant := 16#85#; -- 1 SLEB128 offset base reg
   DW_OP_breg22      : constant := 16#86#; -- 1 SLEB128 offset base reg
   DW_OP_breg23      : constant := 16#87#; -- 1 SLEB128 offset base reg
   DW_OP_breg24      : constant := 16#88#; -- 1 SLEB128 offset base reg
   DW_OP_breg25      : constant := 16#89#; -- 1 SLEB128 offset base reg
   DW_OP_breg26      : constant := 16#8a#; -- 1 SLEB128 offset base reg
   DW_OP_breg27      : constant := 16#8b#; -- 1 SLEB128 offset base reg
   DW_OP_breg28      : constant := 16#8c#; -- 1 SLEB128 offset base reg
   DW_OP_breg29      : constant := 16#8d#; -- 1 SLEB128 offset base reg
   DW_OP_breg30      : constant := 16#8e#; -- 1 SLEB128 offset base reg
   DW_OP_breg31      : constant := 16#8f#; -- 1 SLEB128 offset base reg 0..31
   DW_OP_regx        : constant := 16#90#; -- 1 ULEB128 register
   DW_OP_fbreg       : constant := 16#91#; -- 1 SLEB128 offset
   DW_OP_bregx       : constant := 16#92#; -- 2 ULEB128 reg + SLEB128 offset
   DW_OP_piece       : constant := 16#93#; -- 1 ULEB128 size of piece addressed
   DW_OP_deref_size  : constant := 16#94#; -- 1 1-byte size of data retrieved
   DW_OP_xderef_size : constant := 16#95#; -- 1 1-byte size of data retrieved
   DW_OP_nop         : constant := 16#96#; -- 0
   DW_OP_push_object_address : constant := 16#97#; -- 0
   DW_OP_call2               : constant := 16#98#; -- 1 2-byte offset of DIE
   DW_OP_call4       : constant := 16#99#; -- 1 4-byte offset of DIE
   DW_OP_call_ref    : constant := 16#9a#; -- 1 4- or 8-byte offset of DIE
   DW_OP_lo_user     : constant := 16#E0#; --
   DW_OP_hi_user     : constant := 16#ff#; --

   DW_ATE_address         : constant := 16#1#;
   DW_ATE_boolean         : constant := 16#2#;
   DW_ATE_complex_float   : constant := 16#3#;
   DW_ATE_float           : constant := 16#4#;
   DW_ATE_signed          : constant := 16#5#;
   DW_ATE_signed_char     : constant := 16#6#;
   DW_ATE_unsigned        : constant := 16#7#;
   DW_ATE_unsigned_char   : constant := 16#8#;
   DW_ATE_imaginary_float : constant := 16#9#;
   DW_ATE_lo_user         : constant := 16#80#;
   DW_ATE_hi_user         : constant := 16#ff#;

   DW_ACCESS_public       : constant := 1;
   DW_ACCESS_protected    : constant := 2;
   DW_ACCESS_private      : constant := 3;

   DW_LANG_C89            : constant := 16#0001#;
   DW_LANG_C              : constant := 16#0002#;
   DW_LANG_Ada83          : constant := 16#0003#;
   DW_LANG_C_plus_plus    : constant := 16#0004#;
   DW_LANG_Cobol74        : constant := 16#0005#;
   DW_LANG_Cobol85        : constant := 16#0006#;
   DW_LANG_Fortran77      : constant := 16#0007#;
   DW_LANG_Fortran90      : constant := 16#0008#;
   DW_LANG_Pascal83       : constant := 16#0009#;
   DW_LANG_Modula2        : constant := 16#000a#;
   DW_LANG_Java           : constant := 16#000b#;
   DW_LANG_C99            : constant := 16#000c#;
   DW_LANG_Ada95          : constant := 16#000d#;
   DW_LANG_Fortran95      : constant := 16#000e#;
   DW_LANG_PLI            : constant := 16#000f#;
   DW_LANG_ObjC           : constant := 16#0010#;
   DW_LANG_ObjC_plus_plus : constant := 16#0011#;
   DW_LANG_UPC            : constant := 16#0012#;
   DW_LANG_D              : constant := 16#0013#;
   DW_LANG_Lo_User        : constant := 16#8000#;
   DW_LANG_Assembler      : constant := 16#8001#;
   DW_LANG_Hi_User        : constant := 16#ffff#;

   DW_LANG_MIPS_Assembler : constant := 32769;
   --  This is what binutils seems to use when assembling an ASM source with -g
   --  (even on non-MIPS architectures).

   DW_ID_case_sensitive   : constant := 0;
   DW_ID_up_case          : constant := 1;
   DW_ID_down_case        : constant := 2;
   DW_ID_case_insensitive : constant := 3;

   DW_CC_normal           : constant := 16#1#;
   DW_CC_program          : constant := 16#2#;
   DW_CC_nocall           : constant := 16#3#;
   DW_CC_lo_user          : constant := 16#40#;
   DW_CC_hi_user          : constant := 16#Ff#;

   DW_INL_not_inlined          : constant := 0;
   DW_INL_inlined              : constant := 1;
   DW_INL_declared_not_inlined : constant := 2;
   DW_INL_declared_inlined     : constant := 3;

   --  Line number information.
   --  Line number standard opcode.
   DW_LNS_copy               : constant Unsigned_8 := 1;
   DW_LNS_advance_pc         : constant Unsigned_8 := 2;
   DW_LNS_advance_line       : constant Unsigned_8 := 3;
   DW_LNS_set_file           : constant Unsigned_8 := 4;
   DW_LNS_set_column         : constant Unsigned_8 := 5;
   DW_LNS_negate_stmt        : constant Unsigned_8 := 6;
   DW_LNS_set_basic_block    : constant Unsigned_8 := 7;
   DW_LNS_const_add_pc       : constant Unsigned_8 := 8;
   DW_LNS_fixed_advance_pc   : constant Unsigned_8 := 9;
   DW_LNS_set_prologue_end   : constant Unsigned_8 := 10;
   DW_LNS_set_epilogue_begin : constant Unsigned_8 := 11;
   DW_LNS_set_isa            : constant Unsigned_8 := 12;

   --  Line number extended opcode.
   DW_LNE_end_sequence       : constant Unsigned_8 := 1;
   DW_LNE_set_address        : constant Unsigned_8 := 2;
   DW_LNE_define_file        : constant Unsigned_8 := 3;
   DW_LNE_set_discriminator  : constant Unsigned_8 := 4;
   DW_LNE_lo_user            : constant Unsigned_8 := 128;
   DW_LNE_HP_source_file_correlation : constant Unsigned_8 := 128;
   DW_LNE_HP_SFC_formfeed            : constant Unsigned_32 := 1;
   DW_LNE_HP_SFC_set_listing_line    : constant Unsigned_32 := 2;
   DW_LNE_HP_SFC_associate           : constant Unsigned_32 := 3;
   DW_LNE_hi_user            : constant Unsigned_8 := 255;

   DW_CFA_advance_loc        : constant Unsigned_8 := 16#40#;
   DW_CFA_advance_loc_min    : constant Unsigned_8 := 16#40#;
   DW_CFA_advance_loc_max    : constant Unsigned_8 := 16#7f#;
   DW_CFA_offset             : constant Unsigned_8 := 16#80#;
   DW_CFA_offset_min         : constant Unsigned_8 := 16#80#;
   DW_CFA_offset_max         : constant Unsigned_8 := 16#Bf#;
   DW_CFA_restore            : constant Unsigned_8 := 16#C0#;
   DW_CFA_restore_min        : constant Unsigned_8 := 16#C0#;
   DW_CFA_restore_max        : constant Unsigned_8 := 16#FF#;
   DW_CFA_nop                : constant Unsigned_8 := 16#00#;
   DW_CFA_set_loc            : constant Unsigned_8 := 16#01#;
   DW_CFA_advance_loc1       : constant Unsigned_8 := 16#02#;
   DW_CFA_advance_loc2       : constant Unsigned_8 := 16#03#;
   DW_CFA_advance_loc4       : constant Unsigned_8 := 16#04#;
   DW_CFA_offset_extended    : constant Unsigned_8 := 16#05#;
   DW_CFA_restore_extended   : constant Unsigned_8 := 16#06#;
   DW_CFA_undefined          : constant Unsigned_8 := 16#07#;
   DW_CFA_same_value         : constant Unsigned_8 := 16#08#;
   DW_CFA_register           : constant Unsigned_8 := 16#09#;
   DW_CFA_remember_state     : constant Unsigned_8 := 16#0a#;
   DW_CFA_restore_state      : constant Unsigned_8 := 16#0b#;
   DW_CFA_def_cfa            : constant Unsigned_8 := 16#0c#;
   DW_CFA_def_cfa_register   : constant Unsigned_8 := 16#0d#;
   DW_CFA_def_cfa_offset     : constant Unsigned_8 := 16#0e#;
   DW_CFA_def_cfa_expression : constant Unsigned_8 := 16#0f#;

   DW_EH_PE_omit    : constant Unsigned_8 := 16#Ff#;
   DW_EH_PE_uleb128 : constant Unsigned_8 := 16#01#;
   DW_EH_PE_udata2  : constant Unsigned_8 := 16#02#;
   DW_EH_PE_udata4  : constant Unsigned_8 := 16#03#;
   DW_EH_PE_udata8  : constant Unsigned_8 := 16#04#;
   DW_EH_PE_sleb128 : constant Unsigned_8 := 16#09#;
   DW_EH_PE_sdata2  : constant Unsigned_8 := 16#0A#;
   DW_EH_PE_sdata4  : constant Unsigned_8 := 16#0B#;
   DW_EH_PE_sdata8  : constant Unsigned_8 := 16#0C#;
   DW_EH_PE_absptr  : constant Unsigned_8 := 16#00#;
   DW_EH_PE_pcrel   : constant Unsigned_8 := 16#10#;
   DW_EH_PE_datarel : constant Unsigned_8 := 16#30#;
   DW_EH_PE_format_mask : constant Unsigned_8 := 16#0f#;
end Dwarf;
