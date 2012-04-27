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
with Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Diagnostics; use Diagnostics;
with Files_Table; use Files_Table;
with Get_SCOs;
with Outputs;     use Outputs;
with Switches;    use Switches;

package body ALI_Files is

   --------------
   -- Load_ALI --
   --------------

   procedure Load_ALI (ALI_Filename : String) is
      Discard : Source_File_Index;
      pragma Unreferenced (Discard);
   begin
      Discard := Load_ALI (ALI_Filename, With_SCOs => False);
   end Load_ALI;

   function Load_ALI
     (ALI_Filename : String;
      With_SCOs    : Boolean) return Source_File_Index
   is
      ALI_File  : File_Type;
      ALI_Index : Source_File_Index;

      Line     : String_Access;
      Index    : Natural;

      Preserve_Control_Flow_Seen : Boolean := False;
      --  Set True if unit has been compiled with -fpreserve-control-flow

      GNAT_eS_Seen               : Boolean := False;
      --  Set True if unit has been compiled with -gnateS (or -fdump-scos)

      Matches : Match_Array (0 .. 10);
      --  For regex matching

      function Match (Index : Integer) return String;
      --  Return Index'th match in Line

      function Getc return Character;
      --  Consume and return next character from Line.
      --  Load next line if at end of line. Return ^Z if at end of file.

      function Nextc return Character;
      --  Peek at next character in Line. Return ^Z if at end of file.

      procedure Skipc;
      --  Skip one character in Line

      function Check_Message (M1, M2 : String_Access) return Boolean;
      --  Return True if either M1 or M2 is null or designates an empty string,
      --  else return True if M1 and M2 designate identical strings.

      -------------------
      -- Check_Message --
      -------------------

      function Check_Message (M1, M2 : String_Access) return Boolean is
      begin
         return False
           or else M1 = null or else M1.all = ""
           or else M2 = null or else M2.all = ""
           or else M1.all = M2.all;
      end Check_Message;

      ----------
      -- Getc --
      ----------

      function Getc return Character is
         Next_Char : constant Character := Nextc;
      begin
         Index := Index + 1;
         if Index > Line'Last + 1 then

            --  Note: normally we should just read the next line from ALI_File
            --  and reset Index. However some older versions of the compiler
            --  generated duplicated SCOs in some cases, so if we get two
            --  successive identical lines, we ignore them and keep reading.

            while not End_Of_File (ALI_File) loop
               declare
                  Next_Line : constant String := Get_Line (ALI_File);
               begin
                  if Next_Line = Line.all then
                     Report
                        ("ignoring duplicate line in ALI file "
                         & ALI_Filename, Kind => Warning);

                  else
                     Free (Line);
                     Line := new String'(Next_Line);
                     Index := 1;
                     exit;
                  end if;
               end;
            end loop;
         end if;
         return Next_Char;
      end Getc;

      -----------
      -- Match --
      -----------

      function Match (Index : Integer) return String is
      begin
         if Matches (Index) = No_Match then
            return "";
         else
            return Line (Matches (Index).First .. Matches (Index).Last);
         end if;
      end Match;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         if Index = Line'Last + 1 then
            return ASCII.LF;

         elsif Index in Line'Range then
            return Line (Index);

         else
            return Character'Val (16#1a#);
         end if;
      end Nextc;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
         C : Character;
         pragma Unreferenced (C);
      begin
         C := Getc;
      end Skipc;

      Current_Unit : Source_File_Index := No_Source_File;

      procedure Get_SCOs_From_ALI is new Get_SCOs;

      Expected_Annotation_Kind : ALI_Annotation_Kind;
      Expected_Annotation_Msg  : String_Access;
      --  Variables for checking of annotation validity: annotations must
      --  come in (Exempt_On, Exempt_Off) pairs, nesting forbidden, and
      --  the Exempt_Off message must be either empty or identical to the
      --  Exempt_On one.

   --  Start of processing for Load_ALI

   begin
      --  First check whether this ALI has been already loaded. We identify
      --  this by the fact that it already has an assigned Source_File_Index.

      ALI_Index := Get_Index_From_Full_Name (ALI_Filename, Insert => False);
      if ALI_Index /= No_Source_File then
         Report
           ("ignoring duplicate ALI file " & ALI_Filename, Kind => Warning);
         return No_Source_File;
      end if;

      ALI_Index := Get_Index_From_Full_Name (ALI_Filename, Insert => True);
      Open (ALI_File, In_File, ALI_Filename);

      --  Check that the first line is a valid ALI V line.
      --  Note: the regex has no trailing $ because when reading a Windows
      --  ALI file on UNIX, we need to account for the presence of a trailing
      --  CR character.

      declare
         use Ada.Strings.Unbounded;

         V_Line    : constant String := Get_Line (ALI_File);
         V_Regexp  : constant String := "^V ""(.*)""";
         V_Matcher : constant Pattern_Matcher := Compile (V_Regexp);

         Error_Msg : Unbounded_String;
      begin
         Match (V_Matcher, V_Line, Matches);
         if Matches (0) = No_Match then
            Error_Msg :=
              To_Unbounded_String
                ("malformed ALI file """ & ALI_Filename & """");

            if V_Line'Length > 3
                 and then
               To_Lower (V_Line (V_Line'Last - 3 .. V_Line'Last)) = ".ali"
            then
               Append
                 (Error_Msg,
                  ASCII.LF
                  & "to load ALIs from list use ""--scos=@"
                  & ALI_Filename & """");
            end if;
            Fatal_Error (To_String (Error_Msg));
         end if;
      end;

      --  Here once the ALI file has been succesfully opened

      if Verbose then
         Put_Line ("Loading SCOs from " & ALI_Filename);
      end if;

      Expected_Annotation_Kind := Exempt_On;
      Expected_Annotation_Msg  := null;

      Scan_ALI : while not End_Of_File (ALI_File) loop
         loop
            Free (Line);
            Line := new String'(Get_Line (ALI_File));
            exit when Line'Length > 0;
         end loop;

         case Line (1) is
            when 'A' =>
               if Line.all = "A -fpreserve-control-flow" then
                  Preserve_Control_Flow_Seen := True;

               elsif Line.all = "A -gnateS"
                       or else
                     Line.all = "A -fdump-scos"
               then
                  GNAT_eS_Seen := True;
               end if;

            when 'U' =>
               declare
                  U_Regexp  : constant String := "[^\t]*\t+([^\t]*)\t";
                  U_Matcher : constant Pattern_Matcher := Compile (U_Regexp);
               begin
                  Match (U_Matcher, Line (3 .. Line'Last), Matches);
                  if Matches (0) /= No_Match then
                     Current_Unit := Get_Index_From_Simple_Name (Match (1));
                  end if;
               end;

            when 'N' =>
               declare
                  N_Regexp  : constant String :=
                                "A([0-9]*):([0-9]*) xcov "
                                  & "([^ ]*)( ""(.*)"")?";
                  N_Matcher : constant Pattern_Matcher := Compile (N_Regexp);
                  Annotation : ALI_Annotation;
                  Valid      : Boolean;
                  Sloc       : Source_Location;
               begin
                  Match (N_Matcher, Line (3 .. Line'Last), Matches);
                  if Matches (0) /= No_Match then
                     Sloc :=
                       (Source_File => Current_Unit,
                        Line        => Integer'Value (Match (1)),
                        Column      => Integer'Value (Match (2)));

                     Valid := True;

                     begin
                        Annotation :=
                          (Kind    => ALI_Annotation_Kind'Value (Match (3)),
                           Message => new String'(Match (5)),
                           others  => <>);
                     exception
                        when Constraint_Error =>
                           Report (Sloc, "bad annotation " & Match (3));
                           Valid := False;
                     end;

                     if Valid then
                        if Annotation.Kind /= Expected_Annotation_Kind then
                           Report (Sloc, "unexpected "
                                   & Annotation.Kind'Img & " "
                                   & Annotation.Message.all
                                   & " (expected "
                                   & Expected_Annotation_Kind'Img
                                   & ")");
                        elsif not Check_Message
                                (Annotation.Message, Expected_Annotation_Msg)
                        then
                           Report (Sloc, "unexpected EXEMPT_OFF "
                                   & Annotation.Message.all
                                   & " (expected "
                                   & Expected_Annotation_Msg.all
                                   & ")");
                        end if;

                        if Annotation.Kind = Exempt_On then
                           if Annotation.Message.all = "" then
                              Report (Sloc, "empty message for EXEMPT_ON");
                           end if;

                           Expected_Annotation_Kind := Exempt_Off;
                           Expected_Annotation_Msg  := Annotation.Message;

                        else
                           Expected_Annotation_Kind := Exempt_On;
                           Expected_Annotation_Msg  := null;
                        end if;

                        ALI_Annotations.Insert
                          (Key => Sloc, New_Item => Annotation);
                     end if;
                  end if;
               end;

            when 'C' =>
               exit Scan_ALI;

            when others =>
               null;
         end case;
      end loop Scan_ALI;

      if Expected_Annotation_Kind = Exempt_Off then
         declare
            use ALI_Annotation_Maps;
            Last_Ann_Cursor : constant Cursor := ALI_Annotations.Last;
            Last_Ann_Sloc   : constant Source_Location :=
                                Key (Last_Ann_Cursor);
            Last_Ann        : constant ALI_Annotation :=
                                Element (Last_Ann_Cursor);
         begin
            Report (Last_Ann_Sloc,
              "missing Exempt_Off " & Last_Ann.Message.all);
         end;
      end if;

      if With_SCOs then
         if not Preserve_Control_Flow_Seen then
            Put_Line
              ("warning: " & ALI_Filename
               & ": unit compiled without -fpreserve-control-flow");
         end if;

         if not GNAT_eS_Seen then
            Put_Line
              ("warning: " & ALI_Filename
               & ": unit compiled without SCO generation");
         end if;

         if End_Of_File (ALI_File) then
            --  No SCOs in this ALI

            ALI_Index := No_Source_File;

         else
            Index := 1;
            Get_SCOs_From_ALI;
         end if;
      end if;

      Close (ALI_File);
      return ALI_Index;
   end Load_ALI;

end ALI_Files;
