------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2006 Tristan Gingold                   --
--                      Copyright (C) 2008, 2009 AdaCore                    --
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
with System; use System;
with Interfaces; use Interfaces;
with Hex_Images; use Hex_Images;
with Sparc_Descs; use Sparc_Descs;

package body Disa_Sparc is
   function Get_Field (Field : Sparc_Fields; V : Unsigned_32)
                      return Unsigned_32;

   function Get_Field (Field : Sparc_Fields; V : Unsigned_32)
                      return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.First - F.Last + 1;
   begin
      return Shift_Right (Shift_Left (V, 31 - F.First), 32 - Len);
   end Get_Field;

   subtype Reg_Type is Unsigned_32 range 0 .. 31;

   type Hex_Map_Type is array (Unsigned_32 range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Map_Type := "0123456789abcdef";

   type Cstring_Acc is access constant String;
   type Cond_Map_Type is array (Unsigned_32 range 0 .. 15) of Cstring_Acc;
   subtype S is String;
   Bicc_Map : constant Cond_Map_Type :=
     (0 => new S'("n"),
      1 => new S'("e"),
      2 => new S'("le"),
      3 => new S'("l"),
      4 => new S'("leu"),
      5 => new S'("cs"),
      6 => new S'("neg"),
      7 => new S'("vs"),
      8 => new S'("a"),
      9 => new S'("ne"),
      10 => new S'("g"),
      11 => new S'("ge"),
      12 => new S'("gu"),
      13 => new S'("cc"),
      14 => new S'("pos"),
      15 => new S'("vc")
      );

   type Format_Type is
      (
       Format_Bad,
       Format_Regimm, --  format 3, rd, rs1, rs2 or imm13
       Format_Fp_Mem,
       Format_Mem_Reg,
       Format_Reg_Mem,
       Format_Mem_Creg,
       Format_Mem,
       Format_Ticc,
--       Format_Rd,     --  format 3, rd only.
--       Format_Copro,  --  format 3, fpu or coprocessor
       Format_Asi     --  format 3, rd, rs1, asi and rs2.
       );

   type Insn_Desc_Type is record
      Name : Cstring_Acc;
      Format : Format_Type;
   end record;

   type Insn_Desc_Array is array (Unsigned_32 range 0 .. 63) of Insn_Desc_Type;
   Insn_Desc_10 : constant Insn_Desc_Array :=
     (
      16#00# => (new S'("add"), Format_Regimm),
      16#01# => (new S'("and"), Format_Regimm),
      16#02# => (new S'("or"), Format_Regimm),
      16#03# => (new S'("xor"), Format_Regimm),
      16#04# => (new S'("sub"), Format_Regimm),
      16#05# => (new S'("andn"), Format_Regimm),
      16#06# => (new S'("orn"), Format_Regimm),
      16#07# => (new S'("xnor"), Format_Regimm),
      16#08# => (new S'("addx"), Format_Regimm),
      16#0A# => (new S'("umul"), Format_Regimm),
      16#0B# => (new S'("smul"), Format_Regimm),
      16#0C# => (new S'("subx"), Format_Regimm),
      16#0E# => (new S'("udiv"), Format_Regimm),
      16#0F# => (new S'("sdiv"), Format_Regimm),

      16#10# => (new S'("addcc"), Format_Regimm),
      16#11# => (new S'("andcc"), Format_Regimm),
      16#12# => (new S'("orcc"), Format_Regimm),
      16#13# => (new S'("xorcc"), Format_Regimm),
      16#14# => (new S'("subcc"), Format_Regimm),
      16#15# => (new S'("andncc"), Format_Regimm),
      16#16# => (new S'("orncc"), Format_Regimm),
      16#17# => (new S'("xnorcc"), Format_Regimm),
      16#18# => (new S'("addxcc"), Format_Regimm),
      16#1A# => (new S'("umulcc"), Format_Regimm),
      16#1B# => (new S'("smulcc"), Format_Regimm),
      16#1C# => (new S'("subxcc"), Format_Regimm),
      16#1E# => (new S'("udivcc"), Format_Regimm),
      16#1F# => (new S'("sdivcc"), Format_Regimm),

      16#25# => (new S'("sll"), Format_Regimm),
      16#26# => (new S'("srl"), Format_Regimm),
      16#27# => (new S'("sra"), Format_Regimm),
      16#29# => (new S'("rdpsr"), Format_Regimm),
      16#2A# => (new S'("rdwim"), Format_Regimm),

      16#31# => (new S'("wrpsr"), Format_Regimm),
      16#32# => (new S'("wrwim"), Format_Regimm),
      16#33# => (new S'("wrtbr"), Format_Regimm),
      16#38# => (new S'("jmpl"), Format_Regimm),
      16#39# => (new S'("rett"), Format_Regimm),
      16#3A# => (new S'("t"), Format_Ticc),
      16#3C# => (new S'("save"), Format_Regimm),
      16#3D# => (new S'("restore"), Format_Regimm),

      others => (null, Format_Bad)
      );

   Insn_Desc_11 : constant Insn_Desc_Array :=
     (
      16#00# => (new S'("ld"), Format_Mem_Reg),
      16#01# => (new S'("ldub"), Format_Mem_Reg),
      16#02# => (new S'("lduh"), Format_Mem_Reg),
      16#03# => (new S'("ldd"), Format_Mem_Reg),
      16#04# => (new S'("st"), Format_Reg_Mem),
      16#05# => (new S'("stb"), Format_Reg_Mem),
      16#07# => (new S'("std"), Format_Reg_Mem),

      16#10# => (new S'("lda"), Format_Asi),
      16#13# => (new S'("ldda"), Format_Asi),

      16#23# => (new S'("ldd"), Format_Fp_Mem),

      16#30# => (new S'("ldc"), Format_Mem_Creg),
      16#31# => (new S'("ldcsr"), Format_Mem),

      others => (null, Format_Bad)
      );

   function Get_Insn_Length (Addr : System.Address) return Positive
   is
      pragma Unreferenced (Addr);
   begin
      return 4;
   end Get_Insn_Length;

   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_LEN.
   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Traces.Pc_Type;
                               Line : out String;
                               Line_Pos : out Natural;
                               Insn_Len : out Natural;
                               Sym : Symbolizer'Class)
   is
      pragma Unreferenced (Sym);
      W : Unsigned_32;

      --  Add CHAR to the line.
      procedure Add (C : Character);
      pragma Inline (Add);

      procedure Add (Str : String);
      procedure Add_HT;
      procedure Disp_Hex (V : Unsigned_32);
      procedure Add_Cond (Str : String);
      procedure Add_Ireg (R : Reg_Type);
      procedure Add_D2 (N : Unsigned_32);
      procedure Add_Freg (R : Reg_Type);
      procedure Disp_Mem;
      procedure Disp_Reg_Imm;
      procedure Disp_Format3 (Map : Insn_Desc_Array);

      procedure Add (C : Character) is
      begin
         if Line_Pos <= Line'Last then
            Line (Line_Pos) := C;
            Line_Pos := Line_Pos + 1;
         end if;
      end Add;

      --  Add STR to the line.
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

      procedure Disp_Hex (V : Unsigned_32) is
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Disp_Hex;

      procedure Add_Cond (Str : String)
      is
         V : Unsigned_32;
      begin
         Add (Str);
         Add (Bicc_Map (Shift_Right (W, 25) and 2#1111#).all);
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

      procedure Add_Ireg (R : Reg_Type)
      is
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

      procedure Add_D2 (N : Unsigned_32)
      is
      begin
         if N >= 10 then
            Add (Hex_Digit (N / 10));
         end if;
         Add (Hex_Digit (N mod 10));
      end Add_D2;

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

      procedure Disp_Mem
      is
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

      procedure Disp_Reg_Imm is
      begin
         if Get_Field (F_I, W) /= 0 then
            Add ("0x");
            Add (Hex_Image (Unsigned_16 ((Get_Field (F_Simm13, W)))));
         else
            Add_Ireg (Get_Field (F_Rs2, W));
         end if;
      end Disp_Reg_Imm;

      procedure Disp_Format3 (Map : Insn_Desc_Array)
      is
         Op3 : constant Unsigned_32 range 0 .. 63 := Get_Field (F_Op3, W);
         Rd : constant Unsigned_32 := Get_Field (F_Rd, W);
      begin
         if Map (Op3).Name = null then
            Add ("unknown op=");
            Add (Hex_Image (Unsigned_8 (Get_Field (F_Op, W))));
            Add (", op3=");
            Add (Hex_Image (Unsigned_8 (Op3)));
            return;
         end if;

         Add (Map (Op3).Name.all);
         case Map (Op3).Format is
            when Format_Regimm =>
               Add_HT;
               Add_Ireg (Rd);
               Add (',');
               Add_Ireg (Shift_Right (W, 14) and 31);
               Add (',');
               Disp_Reg_Imm;
            when Format_Fp_Mem =>
               Add_HT;
               Disp_Mem;
               Add (',');
               Add_Freg (Rd);
            when Format_Mem_Reg =>
               Add_HT;
               Disp_Mem;
               Add (',');
               Add_Ireg (Rd);
            when Format_Reg_Mem =>
               Add_HT;
               Add_Ireg (Rd);
               Add (',');
               Disp_Mem;
            when Format_Ticc =>
               Add (Bicc_Map (Get_Field (F_Cond, W)).all);
               Add_HT;
               Disp_Reg_Imm;
            when others =>
               Add ("unhandled format op=");
               Add (Hex_Image (Unsigned_8 (Get_Field (F_Op, W))));
               Add (", op3=");
               Add (Hex_Image (Unsigned_8 (Op3)));
         end case;
      end Disp_Format3;

   begin
      W := Get_Insn (Addr);
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
                  Add_Cond ("b");
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
                        Add_Ireg (Rd);
                        Add (',');
                        Disp_Hex (Shift_Left (Imm22, 10));
                     end if;
                  end;
               when others =>
                  Add ("unknown op2=");
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
               Rd : Unsigned_32;
            begin
               case Op3 is
                  when 16#02# =>
                     --  or rd,%g0,xx => mov.
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
               Disp_Format3 (Insn_Desc_10);
            end;
         when 2#11# =>
            Disp_Format3 (Insn_Desc_11);
         when others =>
            --  Cannot happen.
            raise Program_Error;
      end case;
   end Disassemble_Insn;

end Disa_Sparc;
