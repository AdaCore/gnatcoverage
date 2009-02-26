------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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

with Traces; use Traces;
with Ada.Containers; use Ada.Containers;

package body Execs_Dbase is

   Exec_Base        : aliased Execs_Maps.Map;
   Exec_Base_Handle : constant Exec_Base_Type := Exec_Base'Access;

   function Get_Exec_Base return Exec_Base_Type  is
   begin
      return Exec_Base_Handle;
   end Get_Exec_Base;

   function Equal (L, R : Exec_Base_Entry) return Boolean is
   begin
      return L.Elf_File_Name = R.Elf_File_Name;
   end Equal;

   procedure Open_Exec
     (Execs     : Exec_Base_Type;
      File_Name : String;
      Exec      : out Exe_File_Acc)
   is
      use Execs_Maps;
      Text_Start     : constant Pc_Type := 0;
      Exec_File_Name : String_Acc := new String'(File_Name);
      Base_Entry     : Exec_Base_Entry;
      Position       : constant Cursor := Find (Execs.all,
                                                Exec_File_Name);
   begin
      if Position /= No_Element then
         Exec := Element (Position).Exec;
         Unchecked_Deallocation (Exec_File_Name);
      else
         Exec := new Exe_File_Type;
         Base_Entry.Elf_File_Name := Exec_File_Name;
         Base_Entry.Exec := Exec;
         Open_File (Exec.all,
                    Exec_File_Name.all,
                    Text_Start);
         Insert (Execs.all, Exec_File_Name, Base_Entry);
         Build_Sections (Exec.all);
         Build_Symbols (Exec.all);
      end if;
   end Open_Exec;

   procedure Insert_Exec
     (Execs     : Exec_Base_Type;
      File_Name : String)
   is
      Ignored_Exec : Exe_File_Acc;
      pragma Unreferenced (Ignored_Exec);
   begin
      Open_Exec (Execs, File_Name, Ignored_Exec);
   end Insert_Exec;

   procedure Build_Routines_Names (Execs : Exec_Base_Type) is
   begin
      --  If there is more than one exec in the base, return an error;
      --  otherwise, we may not know how to handle the ambiguities.
      --  We may want to be more subtle at some point; but for now
      --  it seems reasonable to refuse to deduce the function from
      --  several different exec files.
      if Execs_Maps.Length (Execs.all) /= 1 then
         raise Routine_Name_Ambiguity;
      end if;

      declare
         First      : constant Execs_Maps.Cursor
           := Execs_Maps.First (Execs.all);
         First_Exec : constant Exe_File_Acc
           := Execs_Maps.Element (First).Exec;
      begin
         Build_Routines_Name (First_Exec.all);
      end;
   end Build_Routines_Names;

   function Deprecated_First_Exec
     (Execs : Exec_Base_Type)
     return Exe_File_Acc
   is
      First : constant Execs_Maps.Cursor := Execs_Maps.First (Execs.all);
   begin
      return Execs_Maps.Element (First).Exec;
   end Deprecated_First_Exec;

end Execs_Dbase;
