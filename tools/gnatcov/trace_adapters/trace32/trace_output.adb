------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with GNAT.OS_Lib;

with Binary_Files;   use Binary_Files;
with Outputs;        use Outputs;
with Qemu_Traces;    use Qemu_Traces;
with Traces_Files;   use Traces_Files;

package body Trace_Output is

   ----------
   -- Open --
   ----------

   procedure Open
     (This              : in out QEMU_Trace_Output;
      Output_Trace_Path : String;
      Decision_Map_Path : String := "")
   is
   begin
      Open_Output_Flat_Trace_File (Output_Trace_Path,
                                   This.Desc,
                                   This.Trace_File);
      This.Open := True;

      Traces_Dbase.Init_Base (This.Base);

      if Decision_Map_Path /= "" then
         This.Load_Decision_Map (Decision_Map_Path);
      end if;
   end Open;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (This : in out QEMU_Trace_Output) return Boolean is
      (This.Open);

   ----------------
   -- Push_Entry --
   ----------------

   procedure Push_Entry
     (This        : in out QEMU_Trace_Output;
      Trace_Entry : Traces.Trace_Entry)
   is
   begin
      if This.Keep_History (Trace_Entry.Last) then
         --  If we have to keep history for this trace, we write it directly to
         --  the file.
         Traces_Files.Write_Trace_Entry (This.Desc, Trace_Entry);
      else
         --  Otherwise we store or update the entry in the database
         Update_Entry (This, Trace_Entry);
      end if;
   end Push_Entry;

   ----------------------
   -- Close_Trace_File --
   ----------------------

   procedure Close_Trace_File
     (This : in out QEMU_Trace_Output)
   is
      procedure Write_Entry (E : Trace_Entry);

      procedure Write_Entry (E : Trace_Entry) is
      begin
         Traces_Files.Write_Trace_Entry (This.Desc, E);
      end Write_Entry;
   begin

      --  Write the entries from the database to the file
      Traces_Dbase.Iterate (This.Base, Write_Entry'Access);

      Traces_Files.Close_Trace_File (This.Desc);

      This.Open := False;
   end Close_Trace_File;

   ------------------
   -- Update_Entry --
   ------------------

   procedure Update_Entry (This        : in out QEMU_Trace_Output;
                           Trace_Entry : Traces.Trace_Entry)
   is
   begin
      --  Traces_Dbase.Add_Entry is expected handle the merge of Op if we see
      --  the same entry twice.
      Traces_Dbase.Add_Entry (This.Base,
                              Trace_Entry.First,
                              Trace_Entry.Last,
                              Trace_Entry.Op);
   end Update_Entry;

   -----------------------
   -- Load_Decision_Map --
   -----------------------

   procedure Load_Decision_Map
     (This              : in out QEMU_Trace_Output;
      Decision_Map_Path : String)
   is

      function Load_Shared_Object
        (Trace_File  : Trace_File_Type;
         Filename    : String;
         Signature   : Binary_File_Signature;
         First, Last : Traces.Pc_Type) return Boolean
      is (True);

      procedure Process_Info_Entries
        (Trace_File : Trace_File_Type);

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Boolean;
         E          : Trace_Entry);

      procedure Read_Decision_Map_File is new Read_Trace_File_Gen
        (Shared_Object_Type   => Boolean,
         No_Shared_Object     => False,
         Process_Info_Entries => Process_Info_Entries,
         Load_Shared_Object   => Load_Shared_Object,
         Process_Trace_Entry  => Process_Trace_Entry);

      --------------------------
      -- Process_Info_Entries --
      --------------------------

      procedure Process_Info_Entries
        (Trace_File : Trace_File_Type)
      is
      begin
         case Kind (Trace_File) is
         when Flat | History =>
            Fatal_Error
              (Filename (Trace_File)
               & ": decision map expected, but this is a execution trace");

         when Decision_Map =>
            null;

         when Info =>
            --  If Trace_File's first header has an Info kind, then it is
            --  supposed to have a second header, and Trace_File's kind must
            --  come from this second header. Header reading must have already
            --  ensured that.

            raise Program_Error;
         end case;
      end Process_Info_Entries;

      -------------------------
      -- Process_Trace_Entry --
      -------------------------

      procedure Process_Trace_Entry
        (Trace_File : Trace_File_Type;
         SO         : Boolean;
         E          : Trace_Entry)
      is
         pragma Unreferenced (Trace_File);
         pragma Unreferenced (SO);
      begin
         This.Decision_Map.Insert (E.Last);
      end Process_Trace_Entry;

      Trace_File  : Trace_File_Type;
   begin

      if GNAT.OS_Lib.Is_Regular_File (Decision_Map_Path) then
         This.Decision_Map.Clear;

         Read_Decision_Map_File (Decision_Map_Path, Trace_File);

         Free (Trace_File);
      end if;
   end Load_Decision_Map;

   ------------------
   -- Keep_History --
   ------------------

   function Keep_History (This : in out QEMU_Trace_Output;
                          Pc   : Pc_Type) return Boolean
   is
      use Address_Set;
   begin
      return This.Decision_Map.Find (Pc) /= No_Element;
   end Keep_History;

end Trace_Output;
