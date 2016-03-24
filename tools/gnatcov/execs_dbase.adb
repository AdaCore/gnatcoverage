------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;

with Outputs;      use Outputs;

package body Execs_Dbase is

   Exec_Base : Execs_Maps.Map;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Exec_Base_Entry) return Boolean is
   begin
      return L.Exec_File_Name = R.Exec_File_Name;
   end "=";

   ---------------
   -- Open_Exec --
   ---------------

   procedure Open_Exec
     (File_Name  : String;
      Text_Start : Pc_Type;
      Exec       : out Exe_File_Acc)
   is
      use Execs_Maps;
      Base_Entry : Exec_Base_Entry;
      Position   : constant Cursor :=
        Exec_Base.Find (File_Name'Unrestricted_Access);

   begin
      if Position /= No_Element then
         Exec := Element (Position).Exec;
      else
         Exec := Open_File (File_Name, Text_Start);
         Base_Entry.Exec_File_Name := new String'(File_Name);
         Base_Entry.Exec := Exec;
         Exec_Base.Insert (Base_Entry.Exec_File_Name, Base_Entry);
         Build_Sections (Exec.all);
         Build_Symbols (Exec.all);
      end if;
   end Open_Exec;

   -------------------------
   -- Open_Exec_For_Trace --
   -------------------------

   procedure Open_Exec_For_Trace
     (Filename       : String;
      Text_Start     : Pc_Type;
      Trace_Filename : String;
      Signature      : Binary_File_Signature;
      Exec           : out Exe_File_Acc)
   is
   begin
      Open_Exec (Filename, Text_Start, Exec);
      declare
         Exec_Sig        : constant Binary_File_Signature :=
            Get_Signature (Exec.all);
         Mismatch_Reason : constant String :=
            Match_Signatures (Exec_Sig, Signature);
      begin
         if Mismatch_Reason'Length > 0 then
            Warn
              ("executable file " & Filename
               & " does not seem to match trace file " & Trace_Filename
               & ": " & Mismatch_Reason);
         end if;
      end;
   end Open_Exec_For_Trace;

   ----------------
   -- Close_Exec --
   ----------------

   procedure Close_Exec (File_Name : String) is
      use Execs_Maps;
      Base_Entry : Exec_Base_Entry;
      Position   : Cursor := Exec_Base.Find (File_Name'Unrestricted_Access);
   begin
      pragma Assert (Position /= No_Element);
      Base_Entry := Element (Position);
      Exec_Base.Delete (Position);
      Free (Base_Entry.Exec_File_Name);
      Close_File (Base_Entry.Exec);
   end Close_Exec;

end Execs_Dbase;
