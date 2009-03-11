------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2009, AdaCore                    --
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

with Ada.Containers.Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

package body Traces_Names is

   function Equal (L, R : Subprogram_Info) return Boolean;
   --  Needs comment???

   function Equal (L, R : Subprogram_Info) return Boolean
   is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   package Names_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => String_Acc,
      Element_Type => Subprogram_Info,
      "<" => Less_Than,
      "=" => Equal);

   Names : Names_Maps.Map;
   --  Needs comments???
   --  Needs to be available to clients of this unit???

   procedure Add_Routine_Name (Name : String_Acc) is
   begin
      Names.Insert (Name,
                    Subprogram_Info'(Exec => null,
                                     Insns => null,
                                     Traces => null));
   end Add_Routine_Name;

   procedure Remove_Routine_Name (Name : String_Acc) is
   begin
      Names.Exclude (Name);
   end Remove_Routine_Name;

   procedure Iterate
     (Proc : access procedure (Subp_Name : String_Acc;
                               Subp_Info : in out Subprogram_Info))
   is
      use Names_Maps;

      procedure Process_One (Cur : Cursor);
      --  Call Proc for the element at Cur

      procedure Process_One (Cur : Cursor) is
      begin
         Names.Update_Element (Cur, Proc);
      end Process_One;

   --  Start of processing for Iterate

   begin
      Names.Iterate (Process_One'Access);
   end Iterate;

   procedure Read_Routines_Name_From_Text (Filename : String)
   is
      F : File_Type;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            L : constant String := Get_Line (F);
            Name : String_Acc;
            Cur : Names_Maps.Cursor;
         begin
            if L (L'First) = '#' then
               null;
            else
               Name := new String'(L);
               Cur := Names.Find (Name);
               if Names_Maps.Has_Element (Cur) then
                  Put_Line (Standard_Error,
                            "symbol " & Name.all & " is already defined");
               else
                  Add_Routine_Name (Name);
               end if;
            end if;
         end;
      end loop;
      Close (F);
   exception
      when Name_Error | Status_Error =>
         Put_Line (Standard_Output, "cannot open: " & Filename);
         raise;
   end Read_Routines_Name_From_Text;

   procedure Disp_All_Routines
   is
      use Names_Maps;
      Cur : Cursor;
   begin
      Cur := Names.First;
      while Has_Element (Cur) loop
         Put_Line (Key (Cur).all);
         Next (Cur);
      end loop;
   end Disp_All_Routines;

   function Add_Traces (Routine_Name : String_Acc;
                        Exec : Exe_File_Acc;
                        Content : Binary_Content) return Traces_Base_Acc
   is
      use Names_Maps;
      Cur : Cursor;

      procedure Update (Key : String_Acc; El : in out Subprogram_Info);

      procedure Update (Key : String_Acc; El : in out Subprogram_Info)
      is
         pragma Unreferenced (Key);
      begin
         if El.Insns = null and Content'Length > 0 then
            El.Insns := new Binary_Content'(Content);
            El.Exec := Exec;
         else
            --  FIXME: check the contents are similar
            if Content'Length /= El.Insns.all'Length then
               Put_Line (Standard_Error,
                         "error: different function size for "
                           & Routine_Name.all);
               Put_Line (Standard_Error,
                         " (reference is " & Get_Filename (El.Exec.all)
                           & ", file is " & Get_Filename (Exec.all) & ")");
               raise Consolidation_Error;
            end if;
         end if;
         if El.Traces = null then
            El.Traces := new Traces_Base;
         end if;
      end Update;
   begin
      Cur := Names.Find (Routine_Name);
      if not Has_Element (Cur) then
         return null;
      end if;
      Names.Update_Element (Cur, Update'Access);
      return Element (Cur).Traces;
   end Add_Traces;

end Traces_Names;
