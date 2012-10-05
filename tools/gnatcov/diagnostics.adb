------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
with Ada.Text_IO;             use Ada.Text_IO;

with Coverage.Tags; use Coverage.Tags;
with Files_Table;   use Files_Table;
with Hex_Images;    use Hex_Images;
with Switches;      use Switches;

package body Diagnostics is

   procedure Store_Message (M : Message);
   --  Attach M to the relevant Line_Info structure, if any

   procedure Output_Message (M : Message);
   --  Display M

   -----------
   -- Image --
   -----------

   function Image (M : Message) return String is
      subtype Prefix_Str is String (1 .. 3);
      Prefix : constant array (Report_Kind) of Prefix_Str :=
                 (Notice  => "---",
                  Warning => "***",
                  Error   => "!!!");

      function Kind_Image return String;
      --  Text prefix for Kind, empty for the value Error

      function PC_Image return String;
      --  Image of PC, null string for the value 0

      function SCO_Image return String;
      --  Image of SCO, null string for the value No_SCO_Id

      function Sloc_Image return String;
      --  Image of Sloc, null string for the value No_Location

      function Tag_Image return String;
      --  Tag indication, null string for the value No_SC_Tag

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
            return " @" & Hex_Image (M.PC);
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
            return " " & Image (M.Sloc) & ":";
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
              & Tag_Image
              & ": ";
         else
            return "";
         end if;
      end SCO_Image;

      ---------------
      -- Tag_Image --
      ---------------

      function Tag_Image return String is
      begin
         if M.Tag /= No_SC_Tag then
            return " (from " & Tag_Repository.Tag_Name (M.Tag) & ")";
         else
            return "";
         end if;
      end Tag_Image;

      First : Natural := M.Msg'First;

   --  Start of processing for Image

   begin
      if M.Msg (First) = '^' then
         First := First + 1;
      end if;

      return
        Prefix (M.Kind)   &
        PC_Image          &
        Sloc_Image        &
        " "               &
        Kind_Image        &
        SCO_Image & M.Msg (First .. M.Msg'Last);
   end Image;

   ------------
   -- Report --
   ------------

   procedure Report
     (Exe  : Exe_File_Acc;
      PC   : Pc_Type;
      Msg  : String;
      Kind : Report_Kind := Error)
   is
   begin
      Report (Msg, PC => PC, Sloc => Get_Sloc (Exe.all, PC), Kind => Kind);
   end Report;

   procedure Report
     (Sloc : Source_Location;
      Msg  : String;
      Kind : Report_Kind := Error)
   is
   begin
      Report (Msg, Sloc => Sloc, Kind => Kind);
   end Report;

   procedure Report_Violation
     (SCO  : SCO_Id;
      Tag  : SC_Tag;
      Msg  : String)
   is
      Sloc : Source_Location;
   begin
      --  For an MC/DC violation, the message is attached to the decision for
      --  the benefit of HTML output.

      if Kind (SCO) = Condition then
         Sloc := First_Sloc (Enclosing_Decision (SCO));
      else
         Sloc := First_Sloc (SCO);
      end if;

      Report (Msg, Sloc => Sloc, SCO => SCO, Tag => Tag, Kind => Error);
   end Report_Violation;

   procedure Report
     (Msg  : String;
      PC   : Pc_Type         := No_PC;
      Sloc : Source_Location := No_Location;
      SCO  : SCO_Id          := No_SCO_Id;
      Tag  : SC_Tag          := No_SC_Tag;
      Kind : Report_Kind     := Error)
   is
      M : constant Message :=
            (Kind => Kind,
             PC   => PC,
             Sloc => Sloc,
             SCO  => SCO,
             Tag  => Tag,
             Msg  => new String'(Msg));
   begin
      Output_Message (M);
      Store_Message (M);
   end Report;

   --------------------
   -- Output_Message --
   --------------------

   procedure Output_Message (M : Message) is
   begin
      --  In Verbose mode, output all messages, else output only non-violation
      --  messages of level higher than Notice. Note that violations are
      --  always reported in the annotated sources or report output, so it's
      --  fine to omit them here.

      if Verbose
           or else
         (M.Kind > Notice and then M.SCO = No_SCO_Id)
      then
         Put_Line (Image (M));
      end if;
   end Output_Message;

   -------------------
   -- Store_Message --
   -------------------

   procedure Store_Message (M : Message) is
      LI : constant Line_Info_Access := Get_Line (M.Sloc);
   begin
      if LI = null then
         Detached_Messages.Append (M);
      else
         LI.Messages.Append (M);
      end if;
   end Store_Message;

end Diagnostics;
