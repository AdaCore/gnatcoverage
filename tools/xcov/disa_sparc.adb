------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2006 Tristan Gingold                    --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;  use Interfaces;

with Disa_Common; use Disa_Common;
with Hex_Images;  use Hex_Images;
with Sparc_Descs; use Sparc_Descs;

package body Disa_Sparc is

   function Get_Field
     (Field : Sparc_Fields; V : Unsigned_32) return Unsigned_32;
   --  Needs comment???

   subtype Reg_Type is Unsigned_32 range 0 .. 31;

   type Hex_Map_Type is array (Unsigned_32 range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Map_Type := "0123456789abcdef";

   type Cstring_Acc is access constant String;
   subtype String3 is String (1 .. 3);
   type Cond_Map_Type is array (Unsigned_32 range 0 .. 15) of String3;
   subtype S is String;
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
      (Format_Bad,
       Format_Regimm, --  format 3, rd, rs1, rs2 or imm13
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
--       Format_Rd,     --  format 3, rd only.
--       Format_Copro,  --  format 3, fpu or coprocessor
       Format_Asi);     --  format 3, rd, rs1, asi and rs2.

   type Insn_Desc_Type is record
      Name   : Cstring_Acc;
      Format : Format_Type;
   end record;

   type Insn_Desc_Array is array (Unsigned_32 range <>) of Insn_Desc_Type;
   subtype Insn_Desc_Array_6 is Insn_Desc_Array (0 .. 63);
   Insn_Desc_10 : constant Insn_Desc_Array_6 :=
     (16#00# => (new S'("add"),     Format_Regimm),
      16#01# => (new S'("and"),     Format_Regimm),
      16#02# => (new S'("or"),      Format_Regimm),
      16#03# => (new S'("xor"),     Format_Regimm),
      16#04# => (new S'("sub"),     Format_Regimm),
      16#05# => (new S'("andn"),    Format_Regimm),
      16#06# => (new S'("orn"),     Format_Regimm),
      16#07# => (new S'("xnor"),    Format_Regimm),
      16#08# => (new S'("addx"),    Format_Regimm),
      16#0A# => (new S'("umul"),    Format_Regimm),
      16#0B# => (new S'("smul"),    Format_Regimm),
      16#0C# => (new S'("subx"),    Format_Regimm),
      16#0E# => (new S'("udiv"),    Format_Regimm),
      16#0F# => (new S'("sdiv"),    Format_Regimm),

      16#10# => (new S'("addcc"),   Format_Regimm),
      16#11# => (new S'("andcc"),   Format_Regimm),
      16#12# => (new S'("orcc"),    Format_Regimm),
      16#13# => (new S'("xorcc"),   Format_Regimm),
      16#14# => (new S'("subcc"),   Format_Regimm),
      16#15# => (new S'("andncc"),  Format_Regimm),
      16#16# => (new S'("orncc"),   Format_Regimm),
      16#17# => (new S'("xnorcc"),  Format_Regimm),
      16#18# => (new S'("addxcc"),  Format_Regimm),
      16#1A# => (new S'("umulcc"),  Format_Regimm),
      16#1B# => (new S'("smulcc"),  Format_Regimm),
      16#1C# => (new S'("subxcc"),  Format_Regimm),
      16#1E# => (new S'("udivcc"),  Format_Regimm),
      16#1F# => (new S'("sdivcc"),  Format_Regimm),

      16#25# => (new S'("sll"),     Format_Regimm),
      16#26# => (new S'("srl"),     Format_Regimm),
      16#27# => (new S'("sra"),     Format_Regimm),
      16#29# => (new S'("rdpsr"),   Format_Regimm),
      16#2A# => (new S'("rdwim"),   Format_Regimm),

      16#31# => (new S'("wrpsr"),   Format_Regimm),
      16#32# => (new S'("wrwim"),   Format_Regimm),
      16#33# => (new S'("wrtbr"),   Format_Regimm),

      16#34# => (null,              Format_Bad),   -- Fp
      16#35# => (null,              Format_Bad),   -- Fp

      16#38# => (new S'("jmpl"),    Format_Regimm),
      16#39# => (new S'("rett"),    Format_Regimm),
      16#3A# => (new S'("t"),       Format_Ticc),
      16#3C# => (new S'("save"),    Format_Regimm),
      16#3D# => (new S'("restore"), Format_Regimm),

      others => (null,              Format_Bad)
      );

   Insn_Desc_11 : constant Insn_Desc_Array_6 :=
     (16#00# => (new S'("ld"),    Format_Mem_Rd),
      16#01# => (new S'("ldub"),  Format_Mem_Rd),
      16#02# => (new S'("lduh"),  Format_Mem_Rd),
      16#03# => (new S'("ldd"),   Format_Mem_Rd),
      16#04# => (new S'("st"),    Format_Rd_Mem),
      16#05# => (new S'("stb"),   Format_Rd_Mem),
      16#07# => (new S'("std"),   Format_Rd_Mem),

      16#10# => (new S'("lda"),   Format_Asi),
      16#13# => (new S'("ldda"),  Format_Asi),

      16#20# => (new S'("ldf"),   Format_Mem_Fp),
      16#23# => (new S'("lddf"),  Format_Mem_Fp),
      16#24# => (new S'("stf"),   Format_Fp_Mem),
      16#27# => (new S'("stff"),  Format_Fp_Mem),

      16#30# => (new S'("ldc"),   Format_Mem_Creg),
      16#31# => (new S'("ldcsr"), Format_Mem),

      others => (null, Format_Bad)
      );

   subtype Insn_Desc_Array_9 is Insn_Desc_Array (0 .. 511);
   Insn_Desc_Fp34 : constant Insn_Desc_Array_9 :=
     (
      2#000000001# => (new S'("fmovs"),     Format_Fregrs2_Fregrd),
      2#000000101# => (new S'("fnegs"),     Format_Fregrs2_Fregrd),
      2#000001001# => (new S'("fabss"),     Format_Fregrs2_Fregrd),
      2#001000001# => (new S'("fadds"),     Format_Fregrs2_Fregrd),
      2#001000010# => (new S'("faddd"),     Format_Fregrs2_Fregrd),
      2#001000011# => (new S'("faddx"),     Format_Fregrs2_Fregrd),
      2#001000101# => (new S'("fsubs"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001000110# => (new S'("fsubd"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001000111# => (new S'("fsubx"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001001001# => (new S'("fmuls"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001001010# => (new S'("fmuld"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001001011# => (new S'("fmulx"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#001001110# => (new S'("fdivd"),     Format_Fregrs1_Fregrs2_Fregrd),
      2#011000110# => (new S'("fdtos"),     Format_Fregrs2_Fregrd),
      others => (null, Format_Bad)
     );

   Insn_Desc_Fp35 : constant Insn_Desc_Array_9 :=
     (
      2#001010001# => (new S'("fcmps"),      Format_Fregrs1_Fregrs2),
      2#001010010# => (new S'("fcmpd"),      Format_Fregrs1_Fregrs2),
      2#001010011# => (new S'("fcmpx"),      Format_Fregrs1_Fregrs2),
      2#001010101# => (new S'("fcmpes"),     Format_Fregrs1_Fregrs2),
      2#001010110# => (new S'("fcmped"),     Format_Fregrs1_Fregrs2),
      2#001010111# => (new S'("fcmpex"),     Format_Fregrs1_Fregrs2),
      others => (null, Format_Bad)
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
      pragma Unreferenced (Sym);

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
      procedure Disp_Reg_Imm;
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
         while Line_Pos - Line'First < 7 loop
            Add (' ');
         end loop;
      end Add_HT;

      --  Add BYTE to the line.
--       procedure Add_Byte (V : Byte) is
--          type My_Str is array (Natural range 0 .. 15) of Character;
--          Hex_Digit : constant My_Str := "0123456789abcdef";
--       begin
--          Add (Hex_Digit (Natural (Shift_Right (V, 4) and 16#0f#)));
--          Add (Hex_Digit (Natural (Shift_Right (V, 0) and 16#0f#)));
--       end Add_Byte;

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
         Add_Sp (Map (Shift_Right (W, 25) and 2#1111#));
         if (W and 16#2000_0000#) /= 0 then
            Add (",a");
         end if;
         Add_HT;
         Add ("0x");
         V := (W and 16#3f_Ffff#) * 4;
         if (V and 16#80_0000#) /= 0 then
            V := V or 16#Ff00_0000#;
         end if;
         Add (Hex_Image (Pc + V));
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
      -- Disp_Reg_Imm --
      ------------------

      procedure Disp_Reg_Imm is
      begin
         if Get_Field (F_I, W) /= 0 then
            Add ("0x");
            Add (Hex_Image (Unsigned_16 ((Get_Field (F_Simm13, W)))));
         else
            Add_Ireg (Get_Field (F_Rs2, W));
         end if;
      end Disp_Reg_Imm;

      -----------------
      -- Disp_Format --
      -----------------

      procedure Disp_Format (Desc : Insn_Desc_Type) is
         Rd  : constant Unsigned_32 := Get_Field (F_Rd, W);
      begin
         if Desc.Name = null then
            Add ("unknown op=");
            Add (Hex_Image (Unsigned_8 (Get_Field (F_Op, W))));
            Add (", op3=");
            Add (Hex_Image (Unsigned_8 (Get_Field (F_Op3, W))));
            return;
         end if;

         Add (Desc.Name.all);
         case Desc.Format is
            when Format_Regimm =>
               Add_HT;
               Add_Ireg (Rd);
               Add (',');
               Add_Ireg (Shift_Right (W, 14) and 31);
               Add (',');
               Disp_Reg_Imm;

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
               Disp_Reg_Imm;

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

            Add ("call");
            Add_HT;
            Add ("Ox");
            Add (Hex_Image (Pc + Shift_Left (W, 2)));

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
                        Disp_Reg_Imm;
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
                     Disp_Reg_Imm;
                     Add (',');
                     if Rd = 0 then
                        Add ("%y");
                     else
                        Add ("%asr");
                        Add_D2 (Rd);
                     end if;
                     return;

                  when 16#34# =>
                     Disp_Format (Insn_Desc_Fp34 (Get_Field (F_Opf, W)));
                     return;

                  when 16#35# =>
                     Disp_Format (Insn_Desc_Fp35 (Get_Field (F_Opf, W)));
                     return;

                  when 16#38# =>
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
     (Self       : SPARC_Disassembler;
      Insn_Bin   : Binary_Content;
      Pc         : Pc_Type;
      Branch     : out Branch_Kind;
      Flag_Indir : out Boolean;
      Flag_Cond  : out Boolean;
      Dest       : out Pc_Type)
   is
   begin
      raise Program_Error with "not implemented";
   end Get_Insn_Properties;

end Disa_Sparc;
