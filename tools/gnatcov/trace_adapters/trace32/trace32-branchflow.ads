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

with Ada.Text_IO; use Ada.Text_IO;

with Traces;      use Traces;

package Trace32.Branchflow is

   type Branchflow_Trace is tagged limited private;

   type Status_Kind is (Status_Ok,
                        File_Error,
                        Invalid_Trace_File,
                        No_More_Entry);

   function Open (This : in out Branchflow_Trace;
                  Path : String)
                  return Status_Kind
     with Pre  => not This.Is_Open,
     Post => Open'Result in Status_Ok | File_Error | Invalid_Trace_File
               and then
             (if Open'Result = Status_Ok then This.Is_Open);
   --  Open a Trace32 branchflow file for input

   function Is_Open (This : in out Branchflow_Trace) return Boolean;
   --  Return true if the Trace32 branchflow file is open

   procedure Close_Trace_File (This : in out Branchflow_Trace)
     with
       Pre  => This.Is_Open,
       Post => not This.Is_Open;
   --  Close Trace32 branchflow file

   type Branchflow_Trace_Entry is record

      Caller : Pc_Type;
      --  Address of the instruction that causes the change of flow

      Target : Pc_Type;
      --  Destination address of the flow change
   end record;

   function Next_Entry (This : in out Branchflow_Trace;
                        Ent  : out Branchflow_Trace_Entry)
                        return Status_Kind
     with Pre  => This.Is_Open,
          Post => Next_Entry'Result in Status_Ok | No_More_Entry;
   --  Read the next entry in the branchflow file. This function returns
   --  No_More_Entry once there's no more entries to read in the file,

   function Image (Ent : Branchflow_Trace_Entry) return String;
   --  Pretty print a trace entry
private
   type Branchflow_Trace is tagged limited record
      File        : File_Type;
      Open        : Boolean;

      Line_Number : Natural := 0;
      --  Use to display
   end record;
end Trace32.Branchflow;
