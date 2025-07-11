------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;             use Ada.Text_IO;

with Command_Line; use Command_Line;
with Files_Table;  use Files_Table;
with Hex_Images;   use Hex_Images;
with Outputs;
with Switches;     use Switches;

package body Diagnostics is

   function Suppress_Message (M : Message) return Boolean;
   --  Return whether the given message must be removed from reports

   procedure Output_Message (M : Message);
   --  Display M

   procedure Store_Message (M : Message);
   --  Attach M to the relevant Line_Info structure, if any

   -----------
   -- Image --
   -----------

   function Image (M : Message) return String is
      subtype Prefix_Str is String (1 .. 3);
      Prefix : constant array (Report_Kind) of Prefix_Str :=
                 (Notice           => "---",
                  Low_Warning      => "***",
                  Warning          => "***",
                  Error            => "!!!",
                  Info             => ".C.",
                  Violation        => "!C!",
                  Undetermined_Cov => "?C?",
                  Exclusion        => "-C-");

      function Kind_Image return String;
      --  Text prefix for Kind, empty for the value Error

      function PC_Image return String;
      --  Image of PC, null string for the value 0

      function SCO_Image return String;
      --  Image of SCO, null string for the value No_SCO_Id

      function Sloc_Image return String;
      --  Image of Sloc, null string for the value No_Location

      ----------------
      -- Kind_Image --
      ----------------

      function Kind_Image return String is
      begin
         if M.Kind = Error then
            return "";
         else
            return To_Lower (M.Kind'Img) & ": ";
         end if;
      end Kind_Image;

      --------------
      -- PC_Image --
      --------------

      function PC_Image return String is
         use type Pc_Type;
      begin
         if M.PC /= 0 then
            return Ada.Directories.Simple_Name (M.Exe.Get_Filename)
              & "@" & Hex_Image (M.PC);
         else
            return "";
         end if;
      end PC_Image;

      ----------------
      -- Sloc_Image --
      ----------------

      function Sloc_Image return String is
      begin
         if M.Sloc /= No_Location then

            --  Diagnostics can be emitted before the file table is complete,
            --  so we cannot rely on unique filenames, here.

            return " " & Image (M.Violation_Sloc, Unique_Name => False) & ":";
         else
            return "";
         end if;
      end Sloc_Image;

      ---------------
      -- SCO_Image --
      ---------------

      function SCO_Image return String is
      begin
         if M.SCO /= No_SCO_Id then
            return Image (M.SCO, With_Sloc => First_Sloc (M.SCO) /= M.Sloc)
              & ": ";
         else
            return "";
         end if;
      end SCO_Image;

      Msg   : constant String := +M.Msg;
      First : Natural         := Msg'First;

   --  Start of processing for Image

   begin
      if Msg (First) = '^' then
         First := First + 1;
      end if;

      return
        Prefix (M.Kind)   &
        PC_Image          &
        Sloc_Image        &
        " "               &
        Kind_Image        &
        SCO_Image & Msg (First .. Msg'Last);
   end Image;

   ------------
   -- Report --
   ------------

   procedure Report
     (Exe  : Exe_File_Acc;
      PC   : Pc_Type;
      Msg  : String;
      SCO  : SCO_Id := No_SCO_Id;
      Kind : Report_Kind := Error)
   is
      Subprg : constant Address_Info_Acc :=
        Get_Address_Info (Exe.all, Subprogram_Addresses, PC);
      Sloc   : constant Source_Location :=
        (if Subprg = null
         then No_Location
         else Get_Sloc (Subprg.Lines, PC));
   begin
      Report
        (Msg,
         Exe  => Exe,
         PC   => PC,
         Sloc => Sloc,
         SCO  => SCO,
         Kind => Kind);
   end Report;

   procedure Report
     (Sloc : Source_Location;
      Msg  : String;
      Kind : Report_Kind := Error)
   is
   begin
      Report (Msg, Sloc => Sloc, Kind => Kind);
   end Report;

   procedure Report
     (Msg            : String;
      Exe            : Exe_File_Acc    := null;
      PC             : Pc_Type         := No_PC;
      Sloc           : Source_Location := No_Location;
      Violation_Sloc : Source_Location := No_Location;
      SCO            : SCO_Id          := No_SCO_Id;
      Kind           : Report_Kind     := Error)
   is
      M : constant Message :=
        (Kind           => Kind,
         Exe            => Exe,
         PC             => PC,
         Sloc           => Sloc,
         Violation_Sloc => (if Sloc /= No_Location
                            then Sloc
                            else Violation_Sloc),
         SCO            => SCO,
         Msg            => +Msg);
   begin
      Output_Message (M);
      Store_Message (M);
   end Report;

   ---------------------
   -- Report_Coverage --
   ---------------------

   procedure Report_Coverage
     (SCO  : SCO_Id;
      Msg  : String;
      Kind : Coverage_Kind)
   is
      Sloc     : Source_Location;
      --  Sloc of the message

      Violation_Sloc : Source_Location;
      --  Sloc of the violation the message expresses
   begin
      --  For an MC/DC violation, the message is attached to the decision for
      --  the benefit of HTML output.

      if SC_Obligations.Kind (SCO) = Condition then
         Sloc           := Last_Sloc (Enclosing_Decision (SCO));
         Violation_Sloc := First_Sloc (Enclosing_Decision (SCO));
      else
         Sloc           := Last_Sloc (SCO);
         Violation_Sloc := First_Sloc (SCO);
      end if;

      Report (Msg,
              Sloc           => Sloc,
              Violation_Sloc => Violation_Sloc,
              SCO            => SCO,
              Kind           => Kind);
   end Report_Coverage;

   ----------------------
   -- Report_Exclusion --
   ----------------------

   procedure Report_Exclusion
     (SCO : SCO_Id;
      Msg : String)
   is
   begin
      Report_Coverage (SCO, Msg, Kind => Exclusion);
   end Report_Exclusion;

   ----------------------
   -- Report_Violation --
   ----------------------

   procedure Report_Violation
     (SCO : SCO_Id;
      Msg : String)
   is
   begin
      Report_Coverage (SCO, Msg, Kind => Violation);
   end Report_Violation;

   --------------------
   -- Output_Message --
   --------------------

   procedure Output_Message (M : Message) is
   begin
      --  In Verbose mode, output all messages, else output only non-violation
      --  messages of level higher than Notice. Note that violations are
      --  always reported in the annotated sources or report output, so it's
      --  fine to omit them here.

      if Diagnostics_Trace.Is_Active
         or else (M.Kind < Violation and then not Suppress_Message (M))
      then
         if M.Kind in Warning then
            Outputs.Register_Warning;
         end if;
         Put_Line (Image (M));
      end if;
   end Output_Message;

   -------------------
   -- Store_Message --
   -------------------

   procedure Store_Message (M : Message) is
      procedure Append_Message is new Append_To_Array
        (Natural, Message,
         Message_Array, Message_Array_Acc);
   begin
      if not Suppress_Message (M) then
         declare
            LI : constant Line_Info_Access := Get_Line (M.Sloc);
         begin
            if LI /= null then
               Append_Message (LI.Messages, M);
            end if;
         end;
      end if;
   end Store_Message;

   ----------------------
   -- Suppress_Message --
   ----------------------

   function Suppress_Message (M : Message) return Boolean is
   begin
      if Args.Bool_Args (Opt_All_Warnings) then
         return M.Kind <= Notice;
      else
         return M.Kind <= Low_Warning;
      end if;
   end Suppress_Message;

end Diagnostics;
