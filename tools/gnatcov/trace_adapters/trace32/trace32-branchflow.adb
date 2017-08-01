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

with Ada.IO_Exceptions;

with GNAT.Regpat;       use GNAT.Regpat;

with Outputs;

package body Trace32.Branchflow is

   package Pc_Type_IO is new Ada.Text_IO.Modular_IO (Pc_Type);

   Regexp  : constant String :=
     "[[:alpha:]]+:0x([[:alnum:]]+);[[:space:]]+[[:alpha:]]+:0x([[:alnum:]]+)";
   Matcher : constant Pattern_Matcher := Compile (Regexp);

   ---------------------
   -- Open_Trace_File --
   ---------------------

   function Open
     (This : in out Branchflow_Trace;
      Path : String)
      return Status_Kind
   is
      function Check_Header return Boolean;
      --  Return True if the trace header is correct

      ------------------
      -- Check_Header --
      ------------------

      function Check_Header return Boolean is
         Line1 : constant String := Get_Line (This.File);
         Line2 : constant String := Get_Line (This.File);
         Line3 : constant String := Get_Line (This.File);
         Line4 : constant String := Get_Line (This.File);
      begin
         return
           (Line1 = "######################################################" &
              "#################"
            and then
            Line2 = "# Branch Flow trace file"
            and then
            Line3 = "# target; caller; trace record"
            and then
            Line4 = "######################################################" &
              "#################");
      end Check_Header;
   begin
      This.Open := False;

      Open (This.File, In_File, Path);

      This.Open := True;

      if not Check_Header then
         This.Close_Trace_File;
         return Invalid_Trace_File;
      end if;

      This.Line_Number := 4;
      return Status_Ok;
   exception
      when others => return File_Error;
   end Open;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (This : in out Branchflow_Trace) return Boolean is
   begin
      return This.Open;
   end Is_Open;

   ----------------------
   -- Close_Trace_File --
   ----------------------

   procedure Close_Trace_File
     (This : in out Branchflow_Trace)
   is
   begin
      Close (This.File);
      This.Open := False;
   end Close_Trace_File;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (This : in out Branchflow_Trace;
      Ent  : out Branchflow_Trace_Entry)
      return Status_Kind
   is
   begin
      declare
         Line : constant String := Get_Line (This.File);
         --  Trace line format:
         --  C:0x20000434; T:0x20000426
         --      ^ target addr
         --                    ^ caller addr

         Matches : Match_Array (1 .. 2);
         --  For regex matching
      begin
         This.Line_Number := This.Line_Number + 1;

         Match (Matcher, Line, Matches);

         if (for some M of Matches => M = No_Match) then
            Outputs.Warn ("Bad line format in branchflow file (line " &
                            This.Line_Number'Img & ")");
            return No_More_Entry;
         end if;

         declare
            Target_Str : constant String :=
              "16#" & Line (Matches (1).First .. Matches (1).Last) & "#";
            Caller_Str : constant String :=
              "16#" & Line (Matches (2).First .. Matches (2).Last) & "#";
         begin
            Ent.Caller := Pc_Type'Value (Caller_Str);
            Ent.Target := Pc_Type'Value (Target_Str);
         end;

         return Status_Ok;
      end;
   exception
      when Ada.IO_Exceptions.End_Error =>
         Outputs.Warn ("No more BranchFlow entry left");
         return No_More_Entry;
   end Next_Entry;

   -----------
   -- Image --
   -----------

   function Image (Ent : Branchflow_Trace_Entry) return String is
      Caller : String (1 .. (Pc_Type'Size / 4) + 4);
      Target : String (1 .. (Pc_Type'Size / 4) + 4);
   begin
      Pc_Type_IO.Put (Caller, Ent.Caller, Base => 16);
      Pc_Type_IO.Put (Target, Ent.Target, Base => 16);
      return "Caller: " & Caller & " Target:" & Target;
   end Image;

end Trace32.Branchflow;
