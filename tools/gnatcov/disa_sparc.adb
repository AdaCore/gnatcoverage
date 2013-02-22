------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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

with Interfaces;  use Interfaces;

with Disa_Common; use Disa_Common;
with Hex_Images;  use Hex_Images;
with Sparc_Descs; use Sparc_Descs;

package body Disa_Sparc is

   function Get_Field
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32;
   function Get_Field_Sext
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32;
   --  Extract a field from an instruction.

   subtype Reg_Type is Unsigned_32 range 0 .. 31;

   type Hex_Map_Type is array (Unsigned_32 range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Map_Type := "0123456789abcdef";

   subtype String3 is String (1 .. 3);
   subtype String7 is String (1 .. 7);

   type Cond_Map_Type is array (Unsigned_32 range 0 .. 15) of String3;

   Bicc_Map : constant Cond_Map_Type :=
     (0  => "n  ",
      1  => "e  ",
      2  => "le ",
      3  => "l  ",
      4  => "leu",
      5  => "cs ",
      6  => "neg",
      7  => "vs ",
      8  => "a  ",
      9  => "ne ",
      10 => "g  ",
      11 => "ge ",
      12 => "gu ",
      13 => "cc ",
      14 => "pos",
      15 => "vc "
     );

   Fbfcc_Map : constant Cond_Map_Type :=
     (0  => "n  ",
      1  => "ne ",
      2  => "lg ",
      3  => "ul ",
      4  => "l  ",
      5  => "ug ",
      6  => "g  ",
      7  => "u  ",
      8  => "a  ",
      9  => "e  ",
      10 => "ue ",
      11 => "ge ",
      12 => "uge",
      13 => "le ",
      14 => "ule",
      15 => "o  "
     );

   type Format_Type is
     (
      Format_Bad,
      Format_Rd,
      Format_Rs1_Regimm,
      Format_Rs1_Regimm_Rd, --  format 3, rd, rs1, rs2 or imm13
      Format_Fp_Mem,
      Format_Mem_Fp,
      Format_Mem_Rd,
      Format_Rd_Mem,
      Format_Mem_Creg,
      Format_Mem,
      Format_Ticc,
      Format_Fregrs1_Fregrs2,
      Format_Fregrs1_Fregrs2_Fregrd,
      Format_Fregrs2_Fregrd,
      Format_Asi);     --  format 3, rd, rs1, asi and rs2.

   type Insn_Desc_Type is record
      Name   : String7;
      Format : Format_Type;
   end record;

   type Insn_Desc_Array is array (Unsigned_32 range <>) of Insn_Desc_Type;
   subtype Insn_Desc_Array_6 is Insn_Desc_Array (0 .. 63);
   Insn_Desc_10 : constant Insn_Desc_Array_6 :=
     (16#00# => ("add    ", Format_Rs1_Regimm_Rd),
      16#01# => ("and    ", Format_Rs1_Regimm_Rd),
      16#02# => ("or     ", Format_Rs1_Regimm_Rd),
      16#03# => ("xor    ", Format_Rs1_Regimm_Rd),
      16#04# => ("sub    ", Format_Rs1_Regimm_Rd),
      16#05# => ("andn   ", Format_Rs1_Regimm_Rd),
      16#06# => ("orn    ", Format_Rs1_Regimm_Rd),
      16#07# => ("xnor   ", Format_Rs1_Regimm_Rd),
      16#08# => ("addx   ", Format_Rs1_Regimm_Rd),
      16#0A# => ("umul   ", Format_Rs1_Regimm_Rd),
      16#0B# => ("smul   ", Format_Rs1_Regimm_Rd),
      16#0C# => ("subx   ", Format_Rs1_Regimm_Rd),
      16#0E# => ("udiv   ", Format_Rs1_Regimm_Rd),
      16#0F# => ("sdiv   ", Format_Rs1_Regimm_Rd),

      16#10# => ("addcc  ", Format_Rs1_Regimm_Rd),
      16#11# => ("andcc  ", Format_Rs1_Regimm_Rd),
      16#12# => ("orcc   ", Format_Rs1_Regimm_Rd),
      16#13# => ("xorcc  ", Format_Rs1_Regimm_Rd),
      16#14# => ("subcc  ", Format_Rs1_Regimm_Rd),
      16#15# => ("andncc ", Format_Rs1_Regimm_Rd),
      16#16# => ("orncc  ", Format_Rs1_Regimm_Rd),
      16#17# => ("xnorcc ", Format_Rs1_Regimm_Rd),
      16#18# => ("addxcc ", Format_Rs1_Regimm_Rd),
      16#1A# => ("umulcc ", Format_Rs1_Regimm_Rd),
      16#1B# => ("smulcc ", Format_Rs1_Regimm_Rd),
      16#1C# => ("subxcc ", Format_Rs1_Regimm_Rd),
      16#1E# => ("udivcc ", Format_Rs1_Regimm_Rd),
      16#1F# => ("sdivcc ", Format_Rs1_Regimm_Rd),

      16#20# => ("taddcc ", Format_Rs1_Regimm_Rd),
      16#21# => ("tsubcc ", Format_Rs1_Regimm_Rd),
      16#22# => ("taddcct", Format_Rs1_Regimm_Rd),
      16#23# => ("tsubcct", Format_Rs1_Regimm_Rd),
      16#24# => ("mulscc ", Format_Rs1_Regimm_Rd),
      16#25# => ("sll    ", Format_Rs1_Regimm_Rd),
      16#26# => ("srl    ", Format_Rs1_Regimm_Rd),
      16#27# => ("sra    ", Format_Rs1_Regimm_Rd),
      16#28# => ("rdy    ", Format_Rd),
      16#29# => ("rdpsr  ", Format_Rd),
      16#2A# => ("rdwim  ", Format_Rd),
      16#2B# => ("rdtbr  ", Format_Rd),

      16#30# => ("wry    ", Format_Rs1_Regimm),
      16#31# => ("wrpsr  ", Format_Rs1_Regimm),
      16#32# => ("wrwim  ", Format_Rs1_Regimm),
      16#33# => ("wrtbr  ", Format_Rs1_Regimm),

      16#34# => (" fp    ", Format_Bad),   -- Fp
      16#35# => (" fp    ", Format_Bad),   -- Fp

      16#38# => ("jmpl   ", Format_Rs1_Regimm_Rd),
      16#39# => ("rett   ", Format_Rs1_Regimm_Rd),
      16#3A# => ("t      ", Format_Ticc),
      16#3B# => ("iflush ", Format_Mem),
      16#3C# => ("save   ", Format_Rs1_Regimm_Rd),
      16#3D# => ("restore", Format_Rs1_Regimm_Rd),

      others => ("       ", Format_Bad)
      );

   Insn_Desc_11 : constant Insn_Desc_Array_6 :=
     (16#00# => ("ld     ", Format_Mem_Rd),
      16#01# => ("ldub   ", Format_Mem_Rd),
      16#02# => ("lduh   ", Format_Mem_Rd),
      16#03# => ("ldd    ", Format_Mem_Rd),
      16#04# => ("st     ", Format_Rd_Mem),
      16#05# => ("stb    ", Format_Rd_Mem),
      16#06# => ("sth    ", Format_Rd_Mem),
      16#07# => ("std    ", Format_Rd_Mem),
      16#09# => ("ldsb   ", Format_Mem_Rd),
      16#0A# => ("ldsh   ", Format_Mem_Rd),

      16#10# => ("lda    ", Format_Asi),
      16#13# => ("ldda   ", Format_Asi),

      16#20# => ("ldf    ", Format_Mem_Fp),
      16#21# => ("ldfsr  ", Format_Mem),
      16#23# => ("lddf   ", Format_Mem_Fp),
      16#24# => ("stf    ", Format_Fp_Mem),
      16#25# => ("stfsr  ", Format_Mem),
      16#26# => ("stdfq  ", Format_Mem),
      16#27# => ("stff   ", Format_Fp_Mem),

      16#30# => ("ldc    ", Format_Mem_Creg),
      16#31# => ("ldcsr  ", Format_Mem),

      others => ("       ", Format_Bad)
      );

   subtype Insn_Desc_Array_9 is Insn_Desc_Array (0 .. 511);
   Insn_Desc_Fp34 : constant Insn_Desc_Array_9 :=
     (
      2#00000_0001# => ("fmovs  ", Format_Fregrs2_Fregrd),
      2#00000_0101# => ("fnegs  ", Format_Fregrs2_Fregrd),
      2#00000_1001# => ("fabss  ", Format_Fregrs2_Fregrd),
      2#00010_1001# => ("fsqrts ", Format_Fregrs2_Fregrd),
      2#00010_1010# => ("fsqrtd ", Format_Fregrs2_Fregrd),
      2#00010_1011# => ("fsqrtx ", Format_Fregrs2_Fregrd),
      2#00100_0001# => ("fadds  ", Format_Fregrs2_Fregrd),
      2#00100_0010# => ("faddd  ", Format_Fregrs2_Fregrd),
      2#00100_0011# => ("faddx  ", Format_Fregrs2_Fregrd),
      2#00100_0101# => ("fsubs  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_0110# => ("fsubd  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_0111# => ("fsubx  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1001# => ("fmuls  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1010# => ("fmuld  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1011# => ("fmulx  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1101# => ("fdivs  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1110# => ("fdivd  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#00100_1111# => ("fdivx  ", Format_Fregrs1_Fregrs2_Fregrd),
      2#01100_0100# => ("fitos  ", Format_Fregrs2_Fregrd),
      2#01100_0110# => ("fdtos  ", Format_Fregrs2_Fregrd),
      2#01100_0111# => ("fxtos  ", Format_Fregrs2_Fregrd),
      2#01100_1000# => ("fitod  ", Format_Fregrs2_Fregrd),
      2#01100_1001# => ("fstod  ", Format_Fregrs2_Fregrd),
      2#01100_1011# => ("fxtod  ", Format_Fregrs2_Fregrd),
      2#01100_1100# => ("fitox  ", Format_Fregrs2_Fregrd),
      2#01100_1101# => ("fstox  ", Format_Fregrs2_Fregrd),
      2#01100_1110# => ("fdtox  ", Format_Fregrs2_Fregrd),
      2#01101_0001# => ("fstoi  ", Format_Fregrs2_Fregrd),
      2#01101_0010# => ("fdtoi  ", Format_Fregrs2_Fregrd),
      2#01101_0011# => ("fxtoi  ", Format_Fregrs2_Fregrd),
      others       => ("       ", Format_Bad)
     );

   Insn_Desc_Fp35 : constant Insn_Desc_Array_9 :=
     (
      2#00101_0001# => ("fcmps  ", Format_Fregrs1_Fregrs2),
      2#00101_0010# => ("fcmpd  ", Format_Fregrs1_Fregrs2),
      2#00101_0011# => ("fcmpx  ", Format_Fregrs1_Fregrs2),
      2#00101_0101# => ("fcmpes ", Format_Fregrs1_Fregrs2),
      2#00101_0110# => ("fcmped ", Format_Fregrs1_Fregrs2),
      2#00101_0111# => ("fcmpex ", Format_Fregrs1_Fregrs2),
      others       => ("       ", Format_Bad)
     );

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.First - F.Last + 1;
   begin
      return Shift_Right (Shift_Left (V, 31 - F.First), 32 - Len);
   end Get_Field;

   --------------------
   -- Get_Field_Sext --
   --------------------

   function Get_Field_Sext
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.First - F.Last + 1;
   begin
      return Shift_Right_Arithmetic (Shift_Left (V, 31 - F.First), 32 - Len);
   end Get_Field_Sext;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : SPARC_Disassembler;
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

   procedure Disassemble_Insn
     (Self     : SPARC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Line     : out String;
      Line_Pos : out Natural;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Self);

      W : constant Unsigned_32 :=
        To_Big_Endian_U32 (Insn_Bin (Insn_Bin'First .. Insn_Bin'First + 3));

      procedure Add (C : Character);
      pragma Inline (Add);
      --  Add CHAR to the line

      procedure Add (Str : String);
      procedure Add_Sp (Str : String);
      procedure Add_HT;
      procedure Disp_Hex (V : Unsigned_32);
      procedure Add_Cond (Str : String; Map : Cond_Map_Type);
      procedure Add_Ireg (R : Reg_Type);
      procedure Add_D2 (N : Unsigned_32);
      procedure Add_Freg (R : Reg_Type);
      procedure Disp_Mem;
      procedure Disp_Rs2_Imm;
      procedure Disp_Format (Desc : Insn_Desc_Type);
      --  Need comments???

      ---------
      -- Add --
      ---------

      procedure Add (C : Character) is
      begin
         if Line_Pos <= Line'Last then
            Line (Line_Pos) := C;
            Line_Pos := Line_Pos + 1;
         end if;
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add_Sp (Str : String) is
      begin
         for I in Str'Range loop
            exit when Str (I) = ' ';
            Add (Str (I));
         end loop;
      end Add_Sp;

      procedure Add (Str : String) is
      begin
         if Line_Pos + Str'Length <= Line'Last then
            Line (Line_Pos .. Line_Pos + Str'Length - 1) := Str;
            Line_Pos := Line_Pos + Str'Length;
         else
            for I in Str'Range loop
               Add (Str (I));
            end loop;
         end if;
      end Add;

      ------------
      -- Add_HT --
      ------------

      procedure Add_HT is
      begin
         Add (' ');
         while Line_Pos - Line'First < 8 loop
            Add (' ');
         end loop;
      end Add_HT;

      --------------
      -- Disp_Hex --
      --------------

      procedure Disp_Hex (V : Unsigned_32) is
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Disp_Hex;

      --------------
      -- Add_Cond --
      --------------

      procedure Add_Cond (Str : String; Map : Cond_Map_Type) is
         V : Unsigned_32;
      begin
         Add (Str);
         Add_Sp (Map (Get_Field (F_Cond, W)));
         if (W and 16#2000_0000#) /= 0 then
            Add (",a");
         end if;
         Add_HT;
         Add ("0x");
         V := Unsigned_32 (Pc) + Get_Field_Sext (F_Disp22, W) * 4;
         Add (Hex_Image (V));
         Sym.Symbolize (Pc_Type (V), Line, Line_Pos);
      end Add_Cond;

      --------------
      -- Add_Ireg --
      --------------

      procedure Add_Ireg (R : Reg_Type) is
      begin
         Add ('%');
         if R <= 7 then
            Add ('g');

         elsif R <= 15 then
            if R = 14 then
               Add ("sp");
               return;
            else
               Add ('o');
            end if;

         elsif R <= 23 then
            Add ('l');

         else
            if R = 30 then
               Add ("fp");
               return;
            else
               Add ('i');
            end if;
         end if;

         Add (Hex_Digit (R and 7));
      end Add_Ireg;

      ------------
      -- Add_D2 --
      ------------

      procedure Add_D2 (N : Unsigned_32)
      is
      begin
         if N >= 10 then
            Add (Hex_Digit (N / 10));
         end if;
         Add (Hex_Digit (N mod 10));
      end Add_D2;

      --------------
      -- Add_Freg --
      --------------

      procedure Add_Freg (R : Reg_Type)
      is
      begin
         Add ('%');
         Add ('f');
         if R >= 10 then
            Add (Hex_Digit (R / 10));
         end if;
         Add (Hex_Digit (R mod 10));
      end Add_Freg;

      --------------
      -- Disp_Mem --
      --------------

      procedure Disp_Mem is
         Imm : Unsigned_32;
      begin
         Add ('[');
         Add_Ireg (Get_Field (F_Rs1, W));
         if Get_Field (F_I, W) /= 0 then
            Imm := Get_Field (F_Simm13, W);
            if Imm /= 0 then
               Add ('+');
               Add ("0x");
               Add (Hex_Image (Unsigned_16 (Imm)));
            end if;

         else
            Imm := Get_Field (F_Rs2, W);
            if Imm /= 0 then
               Add ('+');
               Add_Ireg (Imm);
            end if;
         end if;
         Add (']');
      end Disp_Mem;

      ------------------
      -- Disp_Rs2_Imm --
      ------------------

      procedure Disp_Rs2_Imm is
      begin
         if Get_Field (F_I, W) /= 0 then
            Add ("0x");
            Add (Hex_Image (Unsigned_16 ((Get_Field (F_Simm13, W)))));
         else
            Add_Ireg (Get_Field (F_Rs2, W));
         end if;
      end Disp_Rs2_Imm;

      -----------------
      -- Disp_Format --
      -----------------

      procedure Disp_Format (Desc : Insn_Desc_Type) is
         Rd  : constant Unsigned_32 := Get_Field (F_Rd, W);
      begin
         if Desc.Name (1) = ' ' then
            Add ("unknown op=");
            Add (Hex_Image (Unsigned_8 (Get_Field (F_Op, W))));
            Add (", op3=");
            Add (Hex_Image (Unsigned_8 (Get_Field (F_Op3, W))));
            return;
         end if;

         Add_Sp (Desc.Name);
         case Desc.Format is
            when Format_Rs1_Regimm_Rd =>
               Add_HT;
               Add_Ireg (Rd);
               Add (',');
               Add_Ireg (Get_Field (F_Rs1, W));
               Add (',');
               Disp_Rs2_Imm;

            when Format_Rs1_Regimm =>
               Add_HT;
               Add_Ireg (Get_Field (F_Rs1, W));
               Add (',');
               Disp_Rs2_Imm;

            when Format_Rd =>
               Add_HT;
               Add_Ireg (Rd);

            when Format_Fp_Mem =>
               Add_HT;
               Add_Freg (Rd);
               Add (',');
               Disp_Mem;

            when Format_Mem_Fp =>
               Add_HT;
               Disp_Mem;
               Add (',');
               Add_Freg (Rd);

            when Format_Mem_Rd =>
               Add_HT;
               Disp_Mem;
               Add (',');
               Add_Ireg (Rd);

            when Format_Rd_Mem =>
               Add_HT;
               Add_Ireg (Rd);
               Add (',');
               Disp_Mem;

            when Format_Ticc =>
               Add_Sp (Bicc_Map (Get_Field (F_Cond, W)));
               Add_HT;
               Disp_Rs2_Imm;

            when Format_Fregrs1_Fregrs2 =>
               Add_HT;
               Add_Freg (Get_Field (F_Rs1, W));
               Add (',');
               Add_Freg (Get_Field (F_Rs2, W));

            when Format_Fregrs2_Fregrd =>
               Add_HT;
               Add_Freg (Get_Field (F_Rs2, W));
               Add (',');
               Add_Freg (Rd);

            when Format_Fregrs1_Fregrs2_Fregrd =>
               Add_HT;
               Add_Freg (Get_Field (F_Rs1, W));
               Add (',');
               Add_Freg (Get_Field (F_Rs2, W));
               Add (',');
               Add_Freg (Rd);

            when others =>
               Add ("unhandled format op=");
               Add (Hex_Image (Unsigned_8 (Get_Field (F_Op, W))));
               Add (", op3=");
               Add (Hex_Image (Unsigned_8 (Get_Field (F_Op3, W))));
         end case;
      end Disp_Format;

   --  Start of processing for Disassemble_Insn

   begin
      Insn_Len := 4;
      Line_Pos := Line'First;

      case Get_Field (F_Op, W) is
         when 2#00# =>

            --  BIcc, SETHI

            case Get_Field (F_Op2, W) is
               when 2#000# =>
                  Add ("unimp");
                  Add_HT;
                  Disp_Hex (Get_Field (F_Disp22, W));

               when 2#010# =>
                  Add_Cond ("b", Bicc_Map);

               when 2#110# =>
                  Add_Cond ("fb", Fbfcc_Map);

               when 2#100# =>
                  declare
                     Rd : constant Unsigned_32 := Get_Field (F_Rd, W);
                     Imm22 : constant Unsigned_32 := Get_Field (F_Imm22, W);
                  begin
                     if Rd = 0 and Imm22 = 0 then
                        Add ("nop");
                     else
                        Add ("sethi");
                        Add_HT;
                        Disp_Hex (Shift_Left (Imm22, 10));
                        Add (',');
                        Add_Ireg (Rd);
                     end if;
                  end;

               when others =>
                  Add ("unknown op=0 op2=");
                  Add (Hex_Image (Get_Field (F_Op2, W)));
            end case;

         when 2#01# =>

            --  Call

            declare
               Val : constant Unsigned_32 := Unsigned_32 (Pc)
                                             + Shift_Left (W, 2);
            begin
               Add ("call");
               Add_HT;
               Add ("0x");
               Add (Hex_Image (Val));
               Sym.Symbolize (Pc_Type (Val), Line, Line_Pos);
            end;

         when 2#10# =>
            declare
               Op3 : constant Unsigned_32 := Get_Field (F_Op3, W);
               Rd  : Unsigned_32;
            begin
               case Op3 is
                  when 16#02# =>

                     --  or rd,%g0,xx => mov

                     if Get_Field (F_Rs1, W) = 0 then
                        Add ("mov");
                        Add_HT;
                        Disp_Rs2_Imm;
                        Add (',');
                        Add_Ireg (Get_Field (F_Rd, W));
                        return;
                     end if;

                  when 16#30# =>
                     Rd := Get_Field (F_Rd, W);
                     Add ("wr");
                     Add_HT;
                     Add_Ireg (Get_Field (F_Rs1, W));
                     Add (',');
                     Disp_Rs2_Imm;
                     Add (',');
                     if Rd = 0 then
                        Add ("%y");
                     else
                        Add ("%asr");
                        Add_D2 (Rd);
                     end if;
                     return;

                  when 16#34# =>
                     declare
                        Opf : constant Unsigned_32 := Get_Field (F_Opf, W);
                     begin
                        if Insn_Desc_Fp34 (Opf).Format = Format_Bad then
                           Add ("unknown op=2 op3=34 opf=");
                           Add (Hex_Image (Unsigned_16 (Opf)));
                        else
                           Disp_Format (Insn_Desc_Fp34 (Opf));
                        end if;
                        return;
                     end;

                  when 16#35# =>
                     declare
                        Opf : constant Unsigned_32 := Get_Field (F_Opf, W);
                     begin
                        if Insn_Desc_Fp35 (Opf).Format = Format_Bad then
                           Add ("unknown op=2 op3=35 opf=");
                           Add (Hex_Image (Unsigned_16 (Opf)));
                        else
                           Disp_Format (Insn_Desc_Fp35 (Opf));
                        end if;
                        return;
                     end;

                  when 16#38# =>
                     --  jmpl
                     if Get_Field (F_Rd, W) = 0
                       and then Get_Field (F_I, W) = 1
                       and then Get_Field (F_Simm13, W) = 8
                     then
                        case Get_Field (F_Rs1, W) is
                           when 15 => -- %o7
                              Add ("retl");
                              return;

                           when 31 => -- %i7
                              Add ("ret");
                              return;

                           when others =>
                              null;
                        end case;
                     end if;

                  when others =>
                     null;
               end case;
               Disp_Format (Insn_Desc_10 (Get_Field (F_Op3, W)));
            end;

         when 2#11# =>
            Disp_Format (Insn_Desc_11 (Get_Field (F_Op3, W)));

         when others =>

            --  Cannot happen

            raise Program_Error;
      end case;
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : SPARC_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      pragma Unreferenced (Self);
      W : Unsigned_32;
      R : Unsigned_32;
   begin
      --  Make sure OUT parameters have a valid value

      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);
      Branch      := Br_None;

      if Insn_Bin'Length < 4 then
         raise Program_Error;
      end if;

      W := To_Big_Endian_U32 (Insn_Bin (Insn_Bin'First .. Insn_Bin'First + 3));

      Flag_Cond := False;
      Flag_Indir := False;

      case Get_Field (F_Op, W) is
         when 2#00# =>
            case Get_Field (F_Op2, W) is
               when 2#010# | 2#110# =>
                  --  Bicc / FBfcc

                  --  Sparc v7 spec:
                  --  [...] the branch is taken, causing a delayed, PC-relative
                  --  control transfer to the address
                  --  PC + (sign extnd (disp22) * 4)
                  Branch_Dest.Target :=
                     Pc + Pc_Type (Get_Field_Sext (F_Disp22, W)) * 4;

                  --  Sparc v7 spec:
                  --  If the branch is not taken, the annul bit field (a) is
                  --  checked.  [...]
                  --  If the branch is taken, the annul field is ignored, and
                  --  the delay instruction is executed.
                  Branch_Dest.Delay_Slot := Pc + 4;

                  FT_Dest.Target := Pc + 8;
                  if Get_Field (F_A, W) = 0 then
                     --  If the annul field is zero, the delay instruction is
                     --  executed.
                     FT_Dest.Delay_Slot := Pc + 4;
                  else
                     --  If a is set, the instruction immediately following
                     --  the branch instruction is not executed (ie, it is
                     --  annulled).
                     FT_Dest.Delay_Slot := No_PC;
                  end if;

                  if (Get_Field (F_Cond, W) and 2#0111#) /= 0 then
                     Flag_Cond := True;
                  elsif Get_Field (F_Cond, W) = 2#1000# then
                     --  BA
                     --  Sparc v7 spec:
                     --  Branch Always (BA), because it always branch
                     --  regardless of the condition code, would normally
                     --  ignore the annul field. Instead it follows the same
                     --  annul field rules:

                     --  Isn't this redundant with the above setting???

                     if Get_Field (F_A, W) = 1 then
                        --  if a = 1, the delay instruction is annulled;
                        Branch_Dest.Delay_Slot := No_PC;
                     else
                        --  if a = 0, the delay instruction is executed.
                        Branch_Dest.Delay_Slot := Pc + 4;
                     end if;
                  end if;
                  Branch := Br_Jmp;
                  return;

               when others =>
                  null;
            end case;

         when 2#01# =>
            --  Call
            --  Sparc v7 spec:
            --  The CALL instruction causes a delayed, unconditionnal,
            --  PC-relative control transfer to the address PC + (disp30 * 4).
            --  [...], therefore the delay slot instruction following the CALL
            --  instruction is always executed.
            Branch_Dest.Target :=
               Pc + Pc_Type (Get_Field_Sext (F_Disp30, W)) * 4;
            Branch_Dest.Delay_Slot := Pc + 4;
            Branch := Br_Call;
            return;

         when 2#10# =>
            if Get_Field (F_Op3, W) = 16#38# then
               Branch_Dest.Delay_Slot := Pc + 4;
               --  jmpl.
               Flag_Indir := True;
               R := Get_Field (F_Rd, W);
               if R = 15 then
                  --  Register indirect call.
                  Branch := Br_Call;
                  return;
               elsif R = 0
                 and then Get_Field (F_I, W) = 1
                 and then Get_Field (F_Simm13, W) = 8
               then
                  --  Ret or retl.
                  R := Get_Field (F_Rs1, W);
                  if R = 15 or else R = 31 then
                     Branch := Br_Ret;
                     return;
                  end if;
               end if;
               --  Unknown.
               Branch := Br_Jmp;
               return;
            end if;

         when others =>
            null;
      end case;

      Branch := Br_None;
   end Get_Insn_Properties;

end Disa_Sparc;
