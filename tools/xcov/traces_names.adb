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
with Interfaces;

with Traces;

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

   procedure Add_Routine_Name
     (Name : String_Acc;
      Exec : Exe_File_Acc := null)
   is
   begin
      Names.Insert (Name,
        Subprogram_Info'(Exec   => Exec,
                         Insns  => null,
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
            if L = "" or else L (L'First) = '#' then
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
         Put_Line (Standard_Error, "cannot open: " & Filename);
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

   -------------------------
   -- Add_Code_And_Traces --
   -------------------------

   procedure Add_Code_And_Traces
     (Routine_Name : String_Acc;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content;
      Base         : access Traces_Base)
   is
      use Names_Maps;
      use Traces;
      use Interfaces;

      procedure Update (Key : String_Acc; Subp_Info : in out Subprogram_Info);

      procedure Update (Key : String_Acc; Subp_Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Key);
         Trace_Cursor : Entry_Iterator;
         Trace        : Trace_Entry;
         First, Last  : Pc_Type;
      begin
         --  First, check if a trace base has already been added to the
         --  subprogram info; if so, check that it does not conflict with
         --  the one given in parameter; if not, initialize the info with
         --  an empty trace.

         if Subp_Info.Insns = null and then Content'Length > 0 then
            Subp_Info.Insns := new Binary_Content'(Content);
            Subp_Info.Exec := Exec;
         else
            --  Check that the Content passed in parameter is the same
            --  as the one we already registered; if the two contents
            --  were different (e.g. came from two executables compiled
            --  with different compilation options), the consolidation would
            --  not make sense.
            --  ??? Checking the actual content is actually quite complicated;
            --  we cannot just compare the binary content, as between two
            --  different executables the same symbol may be located in a
            --  different location. So far, we just make sure that the function
            --  has the same size in the two executables.

            if Content'Length /= Subp_Info.Insns.all'Length then
               Put_Line (Standard_Error,
                         "error: different function size for "
                           & Routine_Name.all);
               Put_Line (Standard_Error,
                         " (reference is " & Get_Filename (Subp_Info.Exec.all)
                           & ", file is " & Get_Filename (Exec.all) & ")");
               raise Consolidation_Error;
            end if;
         end if;

         if Base = null then
            return;
         end if;

         if Subp_Info.Traces = null then
            Subp_Info.Traces := new Traces_Base;
         end if;

         --  Now, update the subprogram traces with the trace base given in
         --  parameter. Rebase the traces' addresses to to subpgrogram address
         --  range (i.e. Subp_Info.Insns'Range).

         Init (Base.all, Trace_Cursor, Content'First);
         Get_Next_Trace (Trace, Trace_Cursor);
         while Trace /= Bad_Trace loop
            exit when Trace.First > Content'Last;

            if Trace.Last >= Content'First then
               if Trace.First >= Content'First then
                  First := Trace.First - Content'First + Subp_Info.Insns'First;
               else
                  First := Subp_Info.Insns'First;
               end if;

               if Trace.Last <= Content'Last then
                  Last := Trace.Last - Content'First + Subp_Info.Insns'First;
               else
                  Last := Subp_Info.Insns'Last;
               end if;

               Add_Entry (Subp_Info.Traces.all, First, Last, Trace.Op);
            end if;

            Get_Next_Trace (Trace, Trace_Cursor);
         end loop;
      end Update;

      Cur : Cursor;
   begin
      Cur := Names.Find (Routine_Name);

      if Has_Element (Cur) then
         Names.Update_Element (Cur, Update'Access);
      end if;
   end Add_Code_And_Traces;

end Traces_Names;
