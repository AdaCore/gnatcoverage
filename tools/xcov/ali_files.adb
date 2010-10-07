------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2009-2010, AdaCore                      --
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

with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Diagnostics; use Diagnostics;
with Files_Table; use Files_Table;
with Get_SCOs;
with Switches;    use Switches;

package body ALI_Files is

   --------------
   -- Load_ALI --
   --------------

   function Load_ALI (ALI_Filename : String) return Source_File_Index is
      ALI_File : File_Type;
      ALI_Index : Source_File_Index;

      Line     : String_Access;
      Index    : Natural;

      --  For regex matching

      Matches : Match_Array (0 .. 10);

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
         if Index > Line'Last + 1 and then not End_Of_File (ALI_File) then
            Free (Line);
            Line := new String'(Get_Line (ALI_File));
            Index := 1;
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

   --  Start of processing for Load_SCOs_From_ALI

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

      --  Here once the ALI file has been succesfully opened

      if Verbose then
         Put_Line ("Loading SCOs from " & ALI_Filename);
      end if;

      Expected_Annotation_Kind := Exempt_On;
      Expected_Annotation_Msg  := null;

      Scan_ALI : loop
         if End_Of_File (ALI_File) then
            --  No SCOs in this ALI

            Close (ALI_File);
            return No_Source_File;
         end if;

         loop
            Free (Line);
            Line := new String'(Get_Line (ALI_File));
            exit when Line'Length > 0;
         end loop;

         case Line (1) is
            when 'U' =>
               declare
                  U_Regexp  : constant String := "[^\t]*\t+([^\t]*)\t";
                  U_Matcher : constant Pattern_Matcher := Compile (U_Regexp);
               begin
                  Match (U_Matcher, Line (3 .. Line'Last), Matches);
                  if Matches (0) /= No_Match then
                     Current_Unit :=  Get_Index_From_Simple_Name (Match (1));
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
                           Message => new String'(Match (5)));
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

      Index := 1;

      Get_SCOs_From_ALI;
      Close (ALI_File);

      return ALI_Index;
   end Load_ALI;

end ALI_Files;
