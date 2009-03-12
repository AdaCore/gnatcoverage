------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
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

with Interfaces; use Interfaces;

with Disa_Common;
with Hex_Images; use Hex_Images;
with Ppc_Descs;  use Ppc_Descs;
with Ppc_Disopc; use Ppc_Disopc;

package body Disa_Ppc is

   function To_Insn (Insn_Bin : Binary_Content) return Unsigned_32
     renames Disa_Common.To_Big_Endian_U32;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : PPC_Disassembler;
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
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Line     : out String;
      Line_Pos : out Natural;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Self);

      W : constant Unsigned_32 := To_Insn (Insn_Bin);

      procedure Add (C : Character);
      pragma Inline (Add);
      --  Add C to the line

      procedure Add (Str : String);
      --  Add Str to the line

      procedure Add_HT;
      --  Comment needed???

      subtype Natural_0_9 is Natural range 0 .. 9;

      procedure Add_Digit (N : Natural_0_9);
      procedure Add_Num2 (Reg : Unsigned_32);
      procedure Add_Simm (Val : Unsigned_32);
      procedure Add_Uimm (Val : Unsigned_32);
      --  Comment needed???

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

      ---------------
      -- Add_Digit --
      ---------------

      procedure Add_Digit (N : Natural_0_9) is
      begin
         Add (Character'Val (Character'Pos ('0') + N));
      end Add_Digit;

      --------------
      -- Add_Num2 --
      --------------

      procedure Add_Num2 (Reg : Unsigned_32) is
         H, L : Unsigned_32;
      begin
         L := Reg mod 10;
         H := Reg / 10;
         if H /= 0 then
            Add_Digit (Natural (H));
         end if;
         Add_Digit (Natural (L));
      end Add_Num2;

      --------------
      -- Add_Simm --
      --------------

      procedure Add_Simm (Val : Unsigned_32) is
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

      --------------
      -- Add_Uimm --
      --------------

      procedure Add_Uimm (Val : Unsigned_32) is
         V : constant Unsigned_16 := Unsigned_16 (Val);
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Add_Uimm;

      Insn_Index : Integer := -1;
      Opc : Natural;

   --  Start of processing for Disassemble_Insn

   begin
      Insn_Len := 4;
      Line_Pos := Line'First;

      --  Find insn

      Opc := Natural (Get_Field (F_OPC, W));

      --  Should be Ppc_Opc_Index (Opc + 1) - 1, but this would miss
      --  the with-update instructions.

      for I in Ppc_Opc_Index (Opc) .. Ppc_Opc_Index (Opc + 1) loop
         if (W and Ppc_Insns (I).Mask) = Ppc_Insns (I).Insn then
            Insn_Index := I;
            exit;
         end if;
      end loop;

      if Insn_Index >= 0 then
         declare
            Insn     : Ppc_Insn_Descr renames Ppc_Insns (Insn_Index);
            Has_Ht   : Boolean := False;
            Is_First : Boolean := True;
            F        : Ppc_Fields;
            Val      : Unsigned_32;
            --  Comments needed???

         begin
            --  Display mnemonic.
            --  This is a bounded string padded with spaces.

            for I in Insn.Name'Range loop
               exit when Insn.Name (I) = ' ';
               Add (Insn.Name (I));
            end loop;

            --  Display fields

            for I in Insn.Fields'Range loop
               F := Insn.Fields (I);
               exit when F = F_Eof;
               if F not in Ppc_Mnemo_Fields then
                  --  Some fields are parts of the mnemonic.
                  --  Add spaces when displaying the first field which is not
                  --  part of the mnemonic.

                  if not Has_Ht then
                     Add_HT;
                     Has_Ht := True;
                     Is_First := False;

                  elsif Is_First then
                     Is_First := False;

                  else
                     --  Add a comma separator between fields.

                     Add (',');
                  end if;

               else
                  pragma Assert (Is_First);
                  null;
               end if;

               --  Extract field value

               Val := Get_Field (F, W);

               --  Display field value

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
                     Add_Num2 (Get_Field (F_A, W));
                     Add (')');
                     exit;

                  when F_SIMM =>
                     Add_Simm (Val);

                  when F_UIMM =>
                     Add_Uimm (Val);

                  when F_LI =>
                     Val := Shift_Left (Val, 2);

                     --  Sign extend

                     if (Val and 16#800000#) /= 0 then
                        Val := Val or 16#Ff000000#;
                     end if;

                     --  Test AA field

                     if (W and 2) = 0 then
                        Val := Val + Pc;
                     end if;

                     Add ("0x");
                     Add (Hex_Image (Val));
                     Sym.Symbolize (Val, Line, Line_Pos);

                  when F_BD =>
                     Val := Shift_Left (Val, 2);

                     --  Sign extend

                     if (Val and 16#8000#) /= 0 then
                        Val := Val or 16#Ffff0000#;
                     end if;

                     --  Test AA field

                     if (W and 2) = 0 then
                        Val := Val + Pc;
                     end if;
                     Add ("0x");
                     Add (Hex_Image (Val));
                     Sym.Symbolize (Val, Line, Line_Pos);

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

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self       : PPC_Disassembler;
      Insn_Bin   : Binary_Content;
      Pc         : Unsigned_32;
      Branch     : out Branch_Kind;
      Flag_Indir : out Boolean;
      Flag_Cond  : out Boolean;
      Dest       : out Unsigned_32)
   is
      pragma Unreferenced (Self);

      Insn : constant Unsigned_32 := To_Insn (Insn_Bin);

      Opc, Xo, Bo : Unsigned_32;
      D : Unsigned_32;
   begin
      Opc := Get_Field (F_OPC, Insn);
      Xo := Get_Field (F_XO, Insn);

      --  To be overriden for non-common cases

      if Get_Field (F_LK, Insn) = 1 then
         Branch := Br_Call;
      else
         Branch := Br_Jmp;
      end if;

      if Opc = 18 then
         --  Opc = 18: b, ba, bl and bla

         Flag_Indir := False;
         Flag_Cond := False;
         D := Shift_Left (Get_Signed_Field (F_LI, Insn), 2);
         if Get_Field (F_AA, Insn) = 1 then
            Dest := D;
         else
            Dest := Pc + D;
         end if;
         return;

      elsif Opc = 16 then
         --  bcx

         Flag_Indir := False;
         D := Shift_Left (Get_Signed_Field (F_BD, Insn), 2);
         if Get_Field (F_AA, Insn) = 1 then
            Dest := D;
         else
            Dest := Pc + D;
         end if;

      elsif Opc = 19 and Xo = 16 then
         --  bclrx

         Flag_Indir := True;
         Dest := 0;
         if Branch = Br_Jmp then
            Branch := Br_Ret;
         end if;

      elsif Opc = 19 and Xo = 528 then
         --  bcctrx

         Flag_Indir := True;
         Dest := 0;

      else
         Branch := Br_None;
         Flag_Indir := False;
         Flag_Cond := False;
         Dest := 0;
         return;
      end if;

      Bo := Get_Field (F_BO, Insn);
      Flag_Cond := not ((Bo and 2#10100#) = 2#10100#);
   end Get_Insn_Properties;

end Disa_Ppc;
