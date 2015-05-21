------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2014, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Disa_Common;  use Disa_Common;
with Hex_Images;
with Highlighting; use Highlighting;

package body Disa_Lmp is

   Invalid_Instruction : exception;

   type Insn_Class is
     (Non_Storage_Relative,
      Non_Storage_Register,
      Storage_Immediate,
      Storage_Register);
   --  GR5/GR6 instruction class. Determines how the instruction is encoded.

   type Operation_Type is mod 2 ** 4;
   type EAM_Op_Type is mod 2 ** 5;
   type F_Inst_Type is mod 2 ** 4;
   --  Instruction fields used to dispatch through decoding tables

   subtype Mnemonic_Type is String (1 .. 8);
   --  Name for an instruction, right-padded with spaces

   type Operand_Kind is
     (Invalid,
      None,
      Block_Move,
      Memory,
      Reg_Src_A, Reg_Src_B, Reg_Dest,
      Reg_F_Src_A, Reg_F_Src_B, Reg_F_Dest, Reg_F_Double,
      F_Inst_Operand, EAM_Operand,
      Cond, Imm, Word_Imm, Signed_Rel);
   --  Instruction operand kind

   type Operand_Array is array (1 .. 3) of Operand_Kind;
   type Insn_Descriptor is record
      Mnemonic : Mnemonic_Type := "INVALID ";
      Sized    : Boolean       := False;
      Operands : Operand_Array := (others => Invalid);
   end record;

   function Invalid (Desc : Insn_Descriptor) return Boolean is
     (Desc.Operands (1) = Invalid);
   --  Return whether Desc is an invalid instruction

   type Operation_Decoding_Table is
     array (Operation_Type) of Insn_Descriptor;
   type EAM_Op_Decoding_Table is
     array (EAM_Op_Type) of Insn_Descriptor;
   type F_Inst_Decoding_Table is
     array (F_Inst_Type) of Insn_Descriptor;
   --  Decoding tables indexed by the Operation/EAM_Op/F_Inst fields

   OP_SUB     : constant Operation_Type := 2#0010#;
   OP_SUBC    : constant Operation_Type := 2#0011#;
   OP_OR      : constant Operation_Type := 2#1001#;
   OP_BRA     : constant Operation_Type := 2#1100#;
   OP_READ    : constant Operation_Type := 2#1111#;
   OP_WRITE   : constant Operation_Type := 2#1111#;

   Non_Storage_Register_Normal_Table : constant Operation_Decoding_Table :=
     (2#0001# => ("bmi     ", False, (Block_Move, None, None)),
      2#0011# => ("bmd     ", False, (Block_Move, None, None)),
      2#0100# => ("dsi     ", False, (None, None, None)),
      2#0101# => ("eni     ", False, (None, None, None)),
      2#0111# => ("rfi     ", False, (None, None, None)),
      2#1111# => ("write   ", True,  (Memory, Reg_Src_B, None)),
      others  => <>);
   Non_Storage_Register_FPU_Table : constant F_Inst_Decoding_Table :=
     (2#0000# => ("fload   ", False, (Reg_F_Dest, Reg_Src_A, None)),
      2#0001# => ("fadd    ", False, (Reg_F_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      2#0010# => ("fsub    ", False, (Reg_F_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      2#0011# => ("fmult   ", False, (Reg_F_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      2#0100# => ("fdiv    ", False, (Reg_F_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      2#0101# => ("fsqrt   ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      2#0110# => ("fneg    ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      2#0111# => ("fabs    ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      2#1000# => ("ftoi    ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      2#1001# => ("itof    ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      2#1100# => ("fmove   ", False, (Reg_F_Dest, Reg_F_Src_A, None)),
      others  => <>);
   Storage_Immediate_Table : constant Operation_Decoding_Table :=
     (2#0000# => ("addi    ", False, (Reg_Src_A, Imm, None)),
      2#0010# => ("subi    ", False, (Reg_Src_A, Imm, None)),
      2#0100# => ("movil   ", False, (Reg_Src_A, Word_Imm, None)),
      2#0101# => ("moviu   ", False, (Reg_Src_A, Word_Imm, None)),
      2#0110# => ("moviq   ", False, (Reg_Src_A, Imm, None)),
      2#1000# => ("wrtl    ", False, (Word_Imm, None, None)),
      2#1001# => ("wrtu    ", False, (Word_Imm, None, None)),
      others  => <>);
   Storage_Register_Normal_Table : constant Operation_Decoding_Table :=
     (2#0000# => ("add     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0001# => ("adc     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0010# => ("sub     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0011# => ("subc    ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0100# => ("extw    ", True,  (Reg_Dest, Reg_Src_A, None)),
      2#0101# => ("asr     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0110# => ("lsr     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#0111# => ("asl     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#1000# => ("xor     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#1001# => ("or      ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#1010# => ("and     ", True,  (Reg_Dest, Reg_Src_A, Reg_Src_B)),
      2#1011# => ("not     ", True,  (Reg_Dest, Reg_Src_A, None)),
      2#1100# => ("bra     ", False, (Cond, Reg_Src_A, Reg_Dest)),
      2#1101# => ("rflag   ", False, (Reg_Dest, None, None)),
      2#1110# => ("extb    ", True,  (Reg_Dest, Reg_Src_A, None)),
      2#1111# => ("read    ", True,  (Reg_Dest, Memory, None)));
   Storage_Register_FPU_Table : constant F_Inst_Decoding_Table :=
     (2#0000# => ("fstore  ", False, (Reg_Dest, Reg_F_Src_A, None)),
      2#1010# => ("fcmp    ", False, (Reg_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      2#1011# => ("fcmpe   ", False, (Reg_Dest, Reg_F_Src_A, Reg_F_Src_B)),
      others  => <>);
   EAM_Write_Table : constant EAM_Op_Decoding_Table :=
     (2#0000# => ("mults   ", False, (Reg_Src_A, Reg_Src_B, None)),
      2#0001# => ("multu   ", False, (Reg_Src_A, Reg_Src_B, None)),
      2#0010# => ("divs    ", False, (Reg_Src_A, None, None)),
      2#0011# => ("divu    ", False, (Reg_Src_A, None, None)),
      2#0100# => ("writemd ", False, (Reg_Src_A, Reg_Src_B, None)),
      2#0101# => ("writemdc", False, (Reg_Src_A, None, None)),
      2#0110# => ("divds   ", False, (Reg_Src_A, None, None)),
      2#0111# => ("divdu   ", False, (Reg_Src_A, None, None)),
      2#1001# => ("asrd    ", False, (Reg_Src_A, None, None)),
      2#1010# => ("lsrd    ", False, (Reg_Src_A, None, None)),
      2#1011# => ("asld    ", False, (Reg_Src_A, None, None)),
      others  => <>);
   EAM_Read_Table : constant EAM_Op_Decoding_Table :=
     (2#0000# => ("readmda ", False, (Reg_Dest, None, None)),
      2#0001# => ("readmdb ", False, (Reg_Dest, None, None)),
      2#0010# => ("readmdc ", False, (Reg_Dest, None, None)),
      others  => <>);

   --  If the EAM_Op field leads to an invalid instruction, disassemble
   --  EAMREAD/EAMWRITE instructions anyway.

   EAM_Write_Desc : constant Insn_Descriptor :=
     ("eamwrite", False, (EAM_Operand, Reg_Src_A, Reg_Src_B));
   EAM_Read_Desc  : constant Insn_Descriptor :=
     ("eamread ", False, (Reg_Dest, EAM_Operand, None));

   --  Likewise for FPU-related instructions

   FP_Inst_Desc   : constant Insn_Descriptor :=
     ("fpinst  ", False, (F_Inst_Operand, Reg_F_Dest, Reg_F_Double));
   FPU_Read_Desc  : constant Insn_Descriptor :=
     ("fpuread ", False, (F_Inst_Operand, Reg_Dest, Reg_F_Double));

   --  Singular instructions: these are not reached through a mere table-based
   --  dispatching.

   Nop_Desc  : constant Insn_Descriptor :=
     ("nop     ", False, (None, None, None));
   Brr_Desc  : constant Insn_Descriptor :=
     ("brr     ", False, (Cond, Signed_Rel, None));
   Move_Desc : constant Insn_Descriptor :=
     ("move    ", True,  (Reg_Dest, Reg_Src_A, None));
   Cmp_Desc  : constant Insn_Descriptor :=
     ("cmp     ", True,  (Reg_Src_A, Reg_Src_B, None));
   Cmpc_Desc : constant Insn_Descriptor :=
     ("cmpc    ", True,  (Reg_Src_A, Reg_Src_B, None));

   type GP_Register is new Natural range 0 .. 31;
   type FP_Register is new Natural range 0 .. 15;
   type Size_Type is (Byte, Word, Long);
   type Condition_Type is new Natural range 0 .. 15;

   COND_FALSE : constant Condition_Type := 2#0000#;
   COND_TRUE  : constant Condition_Type := 2#1111#;

   subtype Condition_Name_Type is String (1 .. 2);
   Condition_Names : constant array (Condition_Type) of Condition_Name_Type :=
     ("fa", "eq", "cs", "os", "ns", "ne", "cc", "oc",
      "nc", "ge", "gt", "hi", "le", "ls", "lt", "tr");

   -------------------------------
   -- Opcode decoding utilities --
   -------------------------------

   type Bit is new Natural range 0 .. 1;
   type Bits_Size is new Natural range 1 .. 16;
   type Bit_Masks is array (Bits_Size) of Unsigned_32;
   Masks : constant Bit_Masks :=
     (16#0001#, 16#0003#, 16#0007#, 16#000f#,
      16#001f#, 16#003f#, 16#007f#, 16#00ff#,
      16#01ff#, 16#03ff#, 16#07ff#, 16#0fff#,
      16#1fff#, 16#3fff#, 16#7fff#, 16#ffff#);

   type Bit_Location is new Integer range -1 .. 31;
   subtype Valid_Bit_Location is Bit_Location range 0 .. Bit_Location'Last;
   --  Location of a single bit in an instruction

   No_Bit_Location : constant Bit_Location := Bit_Location'First;

   type Bits_Location is record
      Start : Bit_Location := No_Bit_Location;
      --  Location of the least significant bit

      Size  : Bits_Size;
      --  Number of bits
   end record;
   --  Location of continuous bits in an instruction

   function Get
     (Insn : Unsigned_32; Location : Valid_Bit_Location) return Bit
   is
     (Bit (Shift_Right (Insn, Natural (Location)) and 1));
   --  Extract and return one bit from an instruction

   function Get
     (Insn : Unsigned_32; Location : Bits_Location) return Unsigned_32
   is
     (Shift_Right (Insn, Natural (Location.Start)) and Masks (Location.Size));
   --  Extract and return bits from an instruction
   --  TODO??? Raise an error if Location invalid

   --  Each field kind may lie in a different location, depending on the
   --  instruction class . The following constants list the different possible
   --  locations for fields. The common ones will not vary depending on the
   --  class. Decoding can then be performed as follows:
   --    - Get the instruction class
   --    - Determine field locations (from the following constants)
   --    - Get discriminants for instruction kinds (Operation and F_Inst
   --      fields).
   --    - Get the instruction descriptor from discriminants and decoding
   --      tables
   --    - Actually disassemble extracting fields (using descriptor and
   --      locations) and format some output

   --  Location of fields in instructions: common fields

   Op_Type_Loc               : constant Bits_Location := (25, 2);

   Operation_Loc             : constant Bits_Location := (21, 4);
   Condition_Loc             : constant Bits_Location := (27, 4);
   F_Inst_Loc                : constant Bits_Location := (27, 4);

   Src_A_Loc                 : constant Bits_Location := (16, 5);
   Src_B_Loc                 : constant Bits_Location := (4, 5);
   Dest_Loc                  : constant Bits_Location := (10, 5);

   F_Src_A_Loc               : constant Bits_Location := (16, 4);
   F_Src_B_Loc               : constant Bits_Location := (4, 4);
   F_Dest_Loc                : constant Bits_Location := (10, 4);

   EAM_Ext_Loc               : constant Bit_Location := 15;
   Src_Imm_Loc               : constant Bit_Location := 9;
   Int_Loc                   : constant Bit_Location := 3;

   Size_Loc                  : constant Bits_Location := (0, 3);

   --  Non-storage relative class instructions fields

   NSRel_Signed_Relative_Loc : constant Bits_Location := (0, 16);

   --  Non-storage register class instructions fields

   NSReg_EAM_Op_Loc          : constant Bits_Location := (10, 5);
   NSReg_Index_Loc           : constant Bits_Location := (10, 5);

   --  Storage register class instructions fields

   SReg_EAM_Op_Loc           : constant Bits_Location := (4, 5);
   SReg_Index_Loc            : constant Bits_Location := (4, 5);
   SReg_Immedate_Loc         : constant Bits_Location := (4, 5);

   --  Storage immediate class instructions fields

   SImm_Immediate_Loc        : constant Bits_Location := (0, 16);

   function Get_Relative_Offset (Insn : Unsigned_32) return Integer_16;
   --  Return the least significant bits of Insn as a signed 16-bit integer.
   --  Can be used to extract the Relative Offset field of non-storage relative
   --  class instructions.

   function Get_Insn_Class (Insn : Unsigned_32) return Insn_Class is
     (Insn_Class'Val (Natural (Get (Insn, Op_Type_Loc))));
   --  Return the instruction class of Insn

   function Get_Operand_Size (Insn : Unsigned_32) return Size_Type is
     (case Get (Insn, Size_Loc) is
         when 2#001# => Byte,
         when 2#010# => Word,
         when 2#100# => Long,
         when others =>
            raise Invalid_Instruction with
        ("Invalid size: " & Unsigned_32'Image (Get (Insn, Size_Loc))));
      --  Return the decoded form of the "size" field in Insn

   -------------------------
   -- Get_Relative_Offset --
   -------------------------

   function Get_Relative_Offset (Insn : Unsigned_32) return Integer_16 is
      Offset : constant Unsigned_16 :=
        Unsigned_16 (Get (Insn, NSRel_Signed_Relative_Loc));
   begin
      return
        (if Offset > 16#7fff#
         then -Integer_16 (not Offset + 1)
         else Integer_16 (Offset));
   end Get_Relative_Offset;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   overriding function Get_Insn_Length
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Insn_Bin);
   begin
      return 4;
   end Get_Insn_Length;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   overriding procedure Disassemble_Insn
     (Self     : LMP_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Sym);

      procedure Put_Comma;
      procedure Put_Register (Prefix : Character; N : Natural);
      procedure Put_Unsigned_Immediate (Imm : Unsigned_16);
      procedure Put_Signed_Immediate (Imm : Integer_16);

      ---------------
      -- Put_Comma --
      ---------------

      procedure Put_Comma is
      begin
         Buffer.Start_Token (Punctuation);
         Buffer.Put (',');
      end Put_Comma;

      ------------------
      -- Put_Register --
      ------------------

      procedure Put_Register (Prefix : Character; N : Natural) is
         Digit_Set : constant array (0 .. 9) of Character := "0123456789";
      begin
         --  There are at most 32 registers, so 2 digits are enough

         pragma Assert (N < 32);
         Buffer.Start_Token (Register);
         Buffer.Put (Prefix);
         if N >= 10 then
            Buffer.Put (Digit_Set (N / 10));
         end if;
         Buffer.Put (Digit_Set (N mod 10));
      end Put_Register;

      ----------------------------
      -- Put_Unsigned_Immediate --
      ----------------------------

      procedure Put_Unsigned_Immediate (Imm : Unsigned_16) is
         Image : constant String := Unsigned_16'Image (Imm);
      begin
         --  Strip the ' ' prefix

         Buffer.Start_Token (Literal);
         Buffer.Put (Image (Image'First + 1 .. Image'Last));
      end Put_Unsigned_Immediate;

      --------------------------
      -- Put_Signed_Immediate --
      --------------------------

      procedure Put_Signed_Immediate (Imm : Integer_16) is
      begin
         Buffer.Start_Token (Literal);
         if Imm = 0 then
            Buffer.Put ('0');
         else
            declare
               Image : String := Integer_16'Image (Imm);
            begin
               if Imm > 0 then
                  Image (Image'First) := '+';
               end if;
               Buffer.Put (Image);
            end;
         end if;
      end Put_Signed_Immediate;

      Insn  : constant Unsigned_32 := To_Big_Endian_U32
        (Slice (Insn_Bin, Pc, Pc + 3));
      Class : constant Insn_Class := Get_Insn_Class (Insn);

      function Get (Location : Bit_Location) return Bit is
        (Get (Insn, Location));
      function Get (Location : Bits_Location) return Unsigned_32 is
        (Get (Insn, Location));

      --  The following locations depend on the class of instruction

      Index_Loc     : Bits_Location;
      Immediate_Loc : Bits_Location;
      EAM_Op_Loc    : Bits_Location;

      --  Instead of extracting fields as soon as we know their locations, only
      --  do so when we need them for the disassembly output: the following
      --  expresion functions enable us to do that while keeping a clear
      --  syntax.

      function Operation return Operation_Type is
        (Operation_Type (Get (Operation_Loc)));
      function Condition return Condition_Type is
        (Condition_Type (Get (Condition_Loc)));
      function F_Inst return F_Inst_Type is
        (F_Inst_Type (Get (F_Inst_Loc)));

      function Src_A return GP_Register is
        (GP_Register (Get (Src_A_Loc)));
      function Src_B return GP_Register is
        (GP_Register (Get (Src_B_Loc)));
      function Dest return GP_Register is
        (GP_Register (Get (Dest_Loc)));

      function F_Src_A return FP_Register is
        (FP_Register (Get (F_Src_A_Loc)));
      function F_Src_B return FP_Register is
        (FP_Register (Get (F_Src_B_Loc)));
      function F_Dest return FP_Register is
        (FP_Register (Get (F_Dest_Loc)));

      function Immediate return Unsigned_16 is
        (Unsigned_16 (Get (Immediate_Loc)));
      function Index return Unsigned_16 is
        (Unsigned_16 (Get (Index_Loc)));
      function Signed_Relative return Integer_16 is
        (Get_Relative_Offset (Insn));
      function EAM_Op return EAM_Op_Type is
        (EAM_Op_Type (Get (EAM_Op_Loc)));

      function EAM_Ext return Bit is
        (Get (EAM_Ext_Loc));
      function Src_Imm return Bit is
        (Get (Src_Imm_Loc));
      function Int return Bit is
        (Get (Int_Loc));

      function Size return Size_Type is
        (Get_Operand_Size (Insn));

      procedure Put_Insn (Desc : Insn_Descriptor);
      --  Format and output the instruction disassembly, given some descriptor
      --  and the non local state.

      --------------
      -- Put_Insn --
      --------------

      procedure Put_Insn (Desc : Insn_Descriptor) is
         Mnemonic_Cur : Natural := Desc.Mnemonic'First;

      begin
         --  Output the mnemonic, stripping ending spaces

         Buffer.Start_Token (Mnemonic);
         while Mnemonic_Cur <= Desc.Mnemonic'Last
           and then Desc.Mnemonic (Mnemonic_Cur) /= ' '
         loop
            Buffer.Put (Desc.Mnemonic (Mnemonic_Cur));
            Mnemonic_Cur := Mnemonic_Cur + 1;
         end loop;

         --  Add the operand size prefix if the instruction needs it

         if Desc.Sized then
            Buffer.Put
              (case Size is
                  when Byte => ".b",
                  when Word => ".w",
                  when Long => ".l");
         end if;

         if Desc.Operands = (None, None, None) then
            return;
         end if;

         --  Pad right with spaces to align operands. Just like objdump, align
         --  operands to 8 characters when possible, or 9 when the mnemonic is
         --  8 characters long.

         Buffer.Start_Token (Text);
         while Buffer.Last_Index < 8 loop
            Buffer.Put (' ');
         end loop;
         if Buffer.Get_Raw (Buffer.Last_Index) /= ' ' then
            Buffer.Put (' ');
         end if;

         for I in Desc.Operands'Range loop
            if Desc.Operands (I) = None then
               exit;
            elsif I > 1 then
               Put_Comma;
            end if;

            case Desc.Operands (I) is
               when Invalid =>
                  Buffer.Start_Token (Highlighting.Error);
                  Buffer.Put ("???");

               when None =>
                  raise Program_Error with "unreachable branch";

               when Block_Move =>
                  Put_Register ('r', 1);
                  Put_Comma;
                  Put_Register ('r', 2);
                  Put_Comma;
                  Put_Register ('r', 3);

               when Memory =>
                  Put_Unsigned_Immediate (Index);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put ('(');
                  Put_Register ('r', Natural (Src_A));
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put (")");

               when Reg_Src_A =>
                  Put_Register ('r', Natural (Src_A));
               when Reg_Src_B =>
                  if Src_Imm = 0 then
                     Put_Register ('r', Natural (Src_B));
                  else
                     Put_Unsigned_Immediate (Immediate);
                  end if;
               when Reg_Dest =>
                  Put_Register ('r', Natural (Dest));

               when Reg_F_Src_A =>
                  Put_Register ('f', Natural (F_Src_A));
               when Reg_F_Src_B =>
                  Put_Register ('f', Natural (F_Src_B));
               when Reg_F_Dest =>
                  Put_Register ('f', Natural (F_Dest));
               when Reg_F_Double =>
                  Put_Register ('f', Natural (F_Src_A));
                  Put_Comma;
                  Put_Register ('f', Natural (F_Src_B));

               when F_Inst_Operand =>
                  Put_Unsigned_Immediate (Unsigned_16 (F_Inst));
               when EAM_Operand =>
                  Put_Unsigned_Immediate (Unsigned_16 (EAM_Op));

               when Cond =>
                  Buffer.Start_Token (Name);
                  Buffer.Put (Condition_Names (Condition));
               when Imm =>
                  Put_Unsigned_Immediate (Immediate);
               when Word_Imm =>
                  Buffer.Start_Token (Literal);
                  Buffer.Put ("0x");
                  Buffer.Put (Hex_Images.Hex_Image (Immediate));
               when Signed_Rel =>
                  Put_Signed_Immediate (Signed_Relative);
            end case;
         end loop;
      end Put_Insn;

   --  Start of processing for Disassemble_Insn

   begin
      case Class is
         when Non_Storage_Relative =>
            if Condition_Type (Get (Condition_Loc)) = COND_FALSE then
               Put_Insn (Nop_Desc);
            else
               Put_Insn (Brr_Desc);
            end if;

         when Non_Storage_Register =>
            EAM_Op_Loc := NSReg_EAM_Op_Loc;
            Index_Loc  := NSReg_Index_Loc;

            if Int = 0 then
               if Operation /= OP_WRITE or else EAM_Ext = 0 then
                  Put_Insn (Non_Storage_Register_Normal_Table (Operation));
               elsif Invalid (EAM_Write_Table (EAM_Op)) then
                  Put_Insn (EAM_Write_Desc);
               else
                  Put_Insn (EAM_Write_Table (EAM_Op));
               end if;

            elsif Invalid (Non_Storage_Register_FPU_Table (F_Inst)) then
               Put_Insn (FP_Inst_Desc);
            else
               Put_Insn (Non_Storage_Register_FPU_Table (F_Inst));
            end if;

         when Storage_Immediate =>
            Immediate_Loc := SImm_Immediate_Loc;
            Put_Insn (Storage_Immediate_Table (Operation));

         when Storage_Register =>
            Index_Loc      := SReg_Index_Loc;
            EAM_Op_Loc     := SReg_EAM_Op_Loc;
            Immediate_Loc  := SReg_Immedate_Loc;

            if Int = 0 then
               if Operation /= OP_READ or else EAM_Ext = 0 then

                  --  Handle various pseudo-instruction: move, cmp

                  if Operation = OP_OR and then Src_B = 0 then
                     Put_Insn (Move_Desc);
                  elsif Operation = OP_SUB and then Dest = 0 then
                     Put_Insn (Cmp_Desc);
                  elsif Operation = OP_SUBC and then Dest = 0 then
                     Put_Insn (Cmpc_Desc);
                  else
                     Put_Insn (Storage_Register_Normal_Table (Operation));
                  end if;

               elsif Invalid (EAM_Read_Table (EAM_Op)) then
                  Put_Insn (EAM_Read_Desc);
               else
                  Put_Insn (EAM_Read_Table (EAM_Op));
               end if;

            elsif Invalid (Storage_Register_FPU_Table (F_Inst)) then
               Put_Insn (FPU_Read_Desc);
            else
               Put_Insn (Storage_Register_FPU_Table (F_Inst));
            end if;
      end case;

      Insn_Len := 4;
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   overriding procedure Get_Insn_Properties
     (Self        : LMP_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      pragma Unreferenced (Self);

      Insn       : constant Unsigned_32 := To_Big_Endian_U32
        (Slice (Insn_Bin, Pc, Pc + 3));
      Class      : constant Insn_Class := Get_Insn_Class (Insn);
      Delay_Slot : constant Pc_Type := Pc + 4;

      function Condition return Condition_Type is
        (Condition_Type (Get (Insn, Condition_Loc)));

   begin
      --  These are default values for control-flow "unrelated" instructions

      Branch      := Br_None;
      Flag_Indir  := False;
      Flag_Cond   := False;
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      --  Look for a control-flow instruction and if Insn_Bin is one, update
      --  its properties. Return without changing anything otherwise.

      case Class is
         when Non_Storage_Relative =>

            --  This is a BRR instruction

            declare
               function Offset_To_PC is new Ada.Unchecked_Conversion
                 (Integer_32, Unsigned_32);

               Offset    : constant Integer_16 :=
                 Get_Relative_Offset (Insn);
               Dest      : constant Pc_Type :=
                 Pc + 4 * (if Offset < 0
                  then Pc_Type (Offset_To_PC (Integer_32 (Offset)))
                  else Pc_Type (Offset));

            begin
               case Condition is
                  when COND_FALSE =>

                     --  The branch is never taken: this is a NOP

                     return;

                  when others =>

                     --  The if branch is always taken: this is a jump,
                     --  otherwise this is a conditional jump.

                     Flag_Cond := Condition /= COND_TRUE;
               end case;
               Branch      := Br_Jmp;
               Branch_Dest := (Dest, Delay_Slot);
               FT_Dest     := (Delay_Slot + 4, Delay_Slot);
            end;

         when Storage_Register =>
            declare
               Operation : constant Operation_Type :=
                 Operation_Type (Get (Insn, Operation_Loc));
            begin
                  if Operation = OP_BRA and then Condition /= COND_FALSE then
                     Branch      := Br_Call;
                     Flag_Indir  := True;
                     Flag_Cond   := Condition /= COND_TRUE;
                     Branch_Dest := (No_PC, Delay_Slot);
                     FT_Dest     := (Delay_Slot + 4, Delay_Slot);
                  end if;
            end;

         when others =>
            null;
      end case;
   end Get_Insn_Properties;

end Disa_Lmp;
