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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;
with Ppc_Descs; use Ppc_Descs;
with Ppc_Opcodes; use Ppc_Opcodes;

procedure Ppc_Genopc is
   --  If no -1, index of the corresponding generic mnemonic.
   Simplified : array (Natural range Ppc_Insns'Range) of Integer
     := (others => -1);

   Masks : array (Ppc_Insns'Range) of Unsigned_32;
begin
   --  Compute Masks.
   for I in Ppc_Insns'Range loop
      declare
         Insn : Ppc_Insn_Descr renames Ppc_Insns (I);
         Mask : Unsigned_32;
      begin
         --  Compute the mask.
         Mask := 0;
         for J in Insn.Fields'Range loop
            exit when Insn.Fields (J) = F_Eof;
            Mask := Mask or Get_Mask (Insn.Fields (J));
         end loop;

         pragma Assert ((Insn.Insn and Mask) = 0);
         Masks (I) := not Mask;
      end;
   end loop;

   --  Fill Simplified.
   declare
      Last : Natural := Ppc_Insns'Last;
   begin
      for I in reverse Ppc_Insns'First .. Ppc_Insns'Last - 1 loop
         if (Ppc_Insns (I).Insn and Masks (Last)) = Ppc_Insns (Last).Insn then
            Simplified (I) := Last;
         else
            Last := I;
         end if;
      end loop;
   end;

   for I in Ppc_Insns'Range loop
      declare
         Insn : Ppc_Insn_Descr renames Ppc_Insns (I);

         function Get_Field (Field : Ppc_Fields) return Unsigned_32
         is
            F : constant Field_Type := Fields_Mask (Field);
            Len : constant Natural := F.Last - F.First + 1;
         begin
            return Shift_Right (Shift_Left (Insn.Insn, F.First), 32 - Len);
         end Get_Field;

         V : Unsigned_32;

         procedure Print_Field_Always (Field : Ppc_Fields; Name : String)
         is
            F : constant Unsigned_32 := Get_Field (Field);
         begin
            if Field /= F_Opc then
               Put (" + ");
            end if;
            Put (Natural (F), 0);
            Put (" * S_" & Name);
            V := V and not Get_Mask (Field);
         end Print_Field_Always;

         procedure Print_Field_If_Nonzero (Field : Ppc_Fields; Name : String)
         is
            F : constant Unsigned_32 := Get_Field (Field);
         begin
            if F = 0 then
               return;
            end if;
            Print_Field_Always (Field, Name);
         end Print_Field_If_Nonzero;

         function Has_Field (Index : Natural; Field : Ppc_Fields)
                            return Boolean is
         begin
            for J in Ppc_Insns (Index).Fields'Range loop
               if Ppc_Insns (Index).Fields (J) = Field then
                  return True;
               end if;
            end loop;
            return False;
         end Has_Field;

         procedure Print_Field_If_Exist (Field : Ppc_Fields; Name : String)
         is
            Prim : Integer;
         begin
            Prim := Simplified (I);
            if Prim < 0 then
               Prim := I;
            end if;
            if Has_Field (Prim, Field) then
               Print_Field_Always (Field, Name);
            end if;
         end Print_Field_If_Exist;

         procedure Print_Field_If_Not_exist (Field : Ppc_Fields; Name : String)
         is
         begin
            if not Has_Field (I, Field) then
               Print_Field_If_Nonzero (Field, Name);
            end if;
         end Print_Field_If_Not_Exist;
      begin
         Put ("      (new S'(""" & Insn.Name.all & """),");
         if Simplified (I) >= 0 then
            Put ("  -- Simplified mnemonic");
         end if;
         New_Line;

         Put ("       ");
         V := Insn.Insn;

         Print_Field_Always (F_Opc, "OPC");
         Print_Field_If_Nonzero (F_Xo, "XO");
         if Simplified (I) >= 0 then
            Print_Field_If_Exist (F_BO, "BO");
            Print_Field_If_Exist (F_BI, "BI");
            Print_Field_If_Exist (F_Spr, "SPR");
            Print_Field_If_Exist (F_LK, "LK");
         else
            Print_Field_If_Not_Exist (F_RC, "RC");
         end if;
         if V /= 0 then
            Put (" XXX");
         end if;
         Put_Line (",");

         Put ("       (");
         for J in Insn.Fields'Range loop
            exit when Insn.Fields (J) = F_Eof;
            Put (Ppc_Fields'Image (Insn.Fields (J)));
            Put (", ");
         end loop;
         Put_Line ("others => F_Eof)),");


         --Insns_Mask (I) := not Mask;
      end;
   end loop;
end Ppc_Genopc;
