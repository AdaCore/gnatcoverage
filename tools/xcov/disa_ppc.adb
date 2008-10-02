------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Hex_Images; use Hex_Images;
with Ppc_Descs; use Ppc_Descs;
with Ppc_Opcodes; use Ppc_Opcodes;

package body Disa_Ppc is
   type Insns_Masks_Type is array (Ppc_Insns'Range) of Unsigned_32;
   Insns_Mask : Insns_Masks_Type;

   procedure Gen_Masks
   is
      --M_Opc : constant Unsigned_32 := Field ((0, 5));
      Mask : Unsigned_32;
      --Prev : Unsigned_32;
      F : Ppc_Fields;
   begin
      --Prev := 0;
      for I in Ppc_Insns'Range loop
         declare
            Insn : Ppc_Insn_Descr renames Ppc_Insns (I);
         begin
            --  Be sure the insns are ordered.
            --pragma Assert (Insn.Insn > Prev,
            --              "insn " & Insn.Name.all & " is not ordered");
            --Prev := Insn.Insn;

            Mask := 0;
            for J in Insn.Fields'Range loop
               F := Insn.Fields (J);
               exit when F = F_Eof;
               Mask := Mask or Get_Mask (Fields_Mask (F));
            end loop;
            Insns_Mask (I) := not Mask;

            --  pragma Assert ((Insn.Insn and Mask) = 0,
            --               "Bad insn for " & Insn.Name.all);
         end;
      end loop;
   end Gen_Masks;

   function Get_Insn_Length (Addr : System.Address) return Positive
   is
      pragma Unreferenced (Addr);
   begin
      return 4;
   end Get_Insn_Length;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   function Get_Insn (Addr : Address) return Unsigned_32
   is
      use System.Storage_Elements;
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      B2 := Read_Byte (Addr + 2);
      B3 := Read_Byte (Addr + 3);
      return Shift_Left (Unsigned_32 (B0), 24)
        or Shift_Left (Unsigned_32 (B1), 16)
        or Shift_Left (Unsigned_32 (B2), 8)
        or Shift_Left (Unsigned_32 (B3), 0);
   end Get_Insn;

   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Traces.Pc_Type;
                               Line : out String;
                               Line_Pos : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type)
   is
      W : Unsigned_32;

      --  Add CHAR to the line.
      procedure Add (C : Character);
      pragma Inline (Add);

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

      procedure Add_Ht is
      begin
         Add (' ');
         while Line_Pos - Line'First < 7 loop
            Add (' ');
         end loop;
      end Add_HT;

      subtype Natural_0_9 is Natural range 0 .. 9;
      procedure Add_Digit (N : Natural_0_9) is
      begin
         Add (Character'Val (Character'Pos ('0') + N));
      end Add_Digit;

      procedure Add_Num2 (Reg : Unsigned_32)
      is
         H, L : Unsigned_32;
      begin
         L := Reg mod 10;
         H := Reg / 10;
         if H /= 0 then
            Add_Digit (Natural (H));
         end if;
         Add_Digit (Natural (L));
      end Add_Num2;


      function Get_Field (F : Field_Type) return Unsigned_32
      is
         Len : constant Natural := F.Last - F.First + 1;
      begin
         return Shift_Right (Shift_Left (W, F.First), 32 - Len);
      end Get_Field;

      procedure Add_Simm (Val : Unsigned_32)
      is
         V : constant Unsigned_16 := Unsigned_16 (Val);
      begin
         if (V and 16#8000#) /= 0 then
            Add ("-0x");
            Add (Hex_Image (-V));
         else
            Add ("0x");
            Add (Hex_Image (V));
         end if;
      end Add_Simm;

      procedure Add_Uimm
      is
         V : constant Unsigned_16 := Unsigned_16 (Get_Field ((16, 31)));
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Add_Uimm;

      pragma Unreferenced (Add_Uimm);

      Insn_Index : Integer := -1;
   begin
      W := Get_Insn (Addr);
      Insn_Len := 4;
      Line_Pos := Line'First;

      --  Find insn.
      for I in Ppc_Insns'Range loop
         if (W and Insns_Mask (I)) = Ppc_Insns (I).Insn then
            Insn_Index := I;
            exit;
         end if;
      end loop;

      if Insn_Index > 0 then
         declare
            Insn : Ppc_Insn_Descr renames Ppc_Insns (Insn_Index);
            Has_Ht : Boolean := False;
            Is_First : Boolean := True;
            F : Ppc_Fields;
            Val : Unsigned_32;
         begin
            Add (Insn.Name.all);
            for I in Insn.Fields'Range loop
               F := Insn.Fields (I);
               exit when F = F_Eof;
               if F not in Ppc_Mnemo_Fields then
                  if not Has_Ht then
                     Add_Ht;
                     Has_Ht := True;
                     Is_First := False;
                  elsif Is_First then
                     Is_First := False;
                  else
                     Add (',');
                  end if;
               else
                  pragma Assert (Is_First);
                  null;
               end if;

               Val := Get_Field (Fields_Mask (F));

               case F is
                  when F_OE =>
                     if Val /= 0 then
                        Add ('o');
                     end if;
                  when F_Rc =>
                     if Val /= 0 then
                        Add ('.');
                     end if;
                  when F_AA =>
                     if Val /= 0 then
                        Add ('a');
                     end if;
                  when F_LK =>
                     if Val /= 0 then
                        Add ('l');
                     end if;
                  when F_Br_Hint =>
                     if Val = 0 then
                        Add ('-');
                     else
                        Add ('+');
                     end if;
                  when F_L =>
                     if Val = 0 then
                        Add ('w');
                     else
                        Add ('d');
                     end if;
                  when F_U =>
                     if Val /= 0 then
                        Add ('u');
                     end if;
                  when F_A | F_D | F_S | F_B =>
                     Add ('r');
                     Add_Num2 (Val);
                  when F_FA | F_FD | F_FS | F_FB | F_FC =>
                     Add ('f');
                     Add_Num2 (Val);
                  when F_Disp =>
                     pragma Assert (Insn.Fields (I + 1) = F_A);
                     pragma Assert (Insn.Fields (I + 2) = F_Eof);
                     Add_Simm (Val);
                     Add ("(r");
                     Add_Num2 (Get_Field (Fields_Mask (F_A)));
                     Add (')');
                     exit;
                  when F_Simm =>
                     Add_Simm (Val);
                  when F_Li =>
                     declare
                        Target : Unsigned_32;
                     begin
                        --  Sign extend
                        if (Val and 16#800000#) /= 0 then
                           Val := Val or 16#Ff000000#;
                        end if;
                        Target := Shift_Left (Val, 2);
                        --  Test AA field.
                        if (W and 2) = 0 then
                           Target := Target + Pc;
                        end if;
                        Add ("0x");
                        Add (Hex_Image (Target));
                        if Proc_Cb /= null then
                           Proc_Cb.all (Target, Line, Line_Pos);
                        end if;
                     end;
                  when F_BD =>
                     declare
                        Target : Unsigned_32;
                     begin
                        --  Sign extend
                        if (Val and 16#8000#) /= 0 then
                           Val := Val or 16#Ffff0000#;
                        end if;
                        Target := Shift_Left (Val, 2);
                        --  Test AA field.
                        if (W and 2) = 0 then
                           Target := Target + Pc;
                        end if;
                        Add ("0x");
                        Add (Hex_Image (Target));
                        if Proc_Cb /= null then
                           Proc_Cb.all (Target, Line, Line_Pos);
                        end if;
                     end;
                  when F_CrfD | F_CrfS =>
                     Add ("cr");
                     Add_Num2 (Val);
                  when F_BI | F_BO | F_SH | F_MB | F_ME =>
                     Add_Num2 (Val);
                  when others =>
                     Add (Hex_Image (Val));
               end case;
            end loop;
         end;
      else
         Add (".long ");
         Add (Hex_Image (W));
      end if;
   end Disassemble_Insn;

begin
   Gen_Masks;
end Disa_Ppc;

