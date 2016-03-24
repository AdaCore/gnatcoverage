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
with Inputs;      use Inputs;
with Outputs;     use Outputs;
with Switches;    use Switches;

package body ALI_Files is

   -----------------------------------------------
   -- Regular expressions for ALI files parsing --
   -----------------------------------------------

   D_Regexp  : constant String := "(([^""\t ]+)|(""([^""]|"""")+""))[\t ]";
   D_Matcher : constant Pattern_Matcher := Compile (D_Regexp);

   N_Regexp  : constant String :=
     "A([0-9]*):([0-9]*)(:[^ ]*)? xcov ([^ ]*)( ""(.*)"")?";
   N_Matcher : constant Pattern_Matcher := Compile (N_Regexp);

   U_Regexp  : constant String := "^[^\t ]*[\t ]+([^\t ]*)";
   U_Matcher : constant Pattern_Matcher := Compile (U_Regexp);

   V_Regexp  : constant String := "^V ""(.*)""$";
   V_Matcher : constant Pattern_Matcher := Compile (V_Regexp);

   function Unquote (Filename : String) return String;
   --  If needed, unquote a filename, such as the ones that can be found on D
   --  lines.

   -------------
   -- Unquote --
   -------------

   function Unquote (Filename : String) return String is
      use Ada.Strings.Unbounded;

      Result   : Unbounded_String;
      In_Quote : Boolean := False;
      --  True when we just met a double quote inside a quoted filename. False
      --  otherwise.

   begin
      if Filename (Filename'First) /= '"' then
         return Filename;
      else
         --  To unquote, just copy the string removing consecutive double
         --  quotes.

         for C of Filename (Filename'First + 1 .. Filename'Last - 1) loop
            if C = '"' then
               if not In_Quote then
                  Append (Result, C);
               end if;
               In_Quote := not In_Quote;
            else
               Append (Result, C);
            end if;
         end loop;
         return To_String (Result);
      end if;
   end Unquote;

   --------------
   -- Load_ALI --
   --------------

   procedure Load_ALI (ALI_Filename : String) is
      Discard_ALI  : Source_File_Index;
      Discard_Units, Discard_Deps : SFI_Vector;
      Discard_Fingerprint : SCOs_Hash;

      pragma Unreferenced (Discard_ALI);
      pragma Warnings (Off, Discard_Units);
      pragma Warnings (Off, Discard_Deps);
      pragma Warnings (Off, Discard_Fingerprint);

   begin
      Discard_ALI :=
        Load_ALI (ALI_Filename, No_CU_Id,
                  Discard_Units, Discard_Deps, Discard_Fingerprint,
                  With_SCOs => False);
   end Load_ALI;

   function Load_ALI
     (ALI_Filename : String;
      CU           : CU_Id;
      Units        : out SFI_Vector;
      Deps         : out SFI_Vector;
      Fingerprint  : out SCOs_Hash;
      With_SCOs    : Boolean) return Source_File_Index
   is
      use GNAT.SHA1;

      ALI_File        : File_Type;
      ALI_Index       : Source_File_Index;
      C_Lines_Ctx     : GNAT.SHA1.Context;

      Line  : String_Access;
      Index : Natural;

      Matches : Match_Array (0 .. 10);
      --  For regex matching

      function Match (Index : Integer) return String;
      --  Return Index'th match in Line

      function Get_Stripped_Line (F : File_Type) return String;
      --  Like Get_Line but strip trailing CR, to allow for processing Windows
      --  LI files on a UNIX host.

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
                  Next_Line : constant String := Get_Stripped_Line (ALI_File);
               begin
                  if Next_Line = Line.all then
                     Report
                        ("ignoring duplicate line in ALI file "
                         & ALI_Filename, Kind => Warning);

                  else
                     Free (Line);
                     Line := new String'(Next_Line);
                     Index := 1;

                     --  If this is a C line, enter it into the SCOs hash

                     if Line'Length > 0 and then Line (Line'First) = 'C' then
                        Update (C_Lines_Ctx, Next_Line);
                     end if;

                     exit;
                  end if;
               end;
            end loop;
         end if;
         return Next_Char;
      end Getc;

      -----------------------
      -- Get_Stripped_Line --
      -----------------------

      function Get_Stripped_Line (F : File_Type) return String is
         Line : constant String := Get_Line (F);
         Last : Integer := Line'Last;
      begin
         if Last in Line'Range and then Line (Last) = ASCII.CR then
            Last := Last - 1;
         end if;
         return Line (Line'First .. Last);
      end Get_Stripped_Line;

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

      procedure Get_SCOs_From_ALI is new Get_SCOs;

      --  Local variables

      No_Object                  : Boolean := False;
      --  Set True if the P line contains the NO flag

      Preserve_Control_Flow_Seen : Boolean := False;
      --  Set True if unit has been compiled with -fpreserve-control-flow

      Dump_SCOs_Seen             : Boolean := False;
      --  Set True if unit has been compiled with -fdump-scos (or -gnateS)

      Debug_Seen                 : Boolean := False;
      --  Set True if unit has been compiled with -g

      Expected_Annotation_Kind : ALI_Annotation_Kind;
      Expected_Annotation_Msg  : String_Access;
      --  Variables for checking of annotation validity: annotations must
      --  come in (Exempt_On, Exempt_Off) pairs, nesting forbidden, and
      --  the Exempt_Off message must be either empty or identical to the
      --  Exempt_On one.

   --  Start of processing for Load_ALI

   begin
      pragma Assert (Deps.Last_Index = 0);

      --  First check whether this ALI has been already loaded. We identify
      --  this by the fact that it already has an assigned Source_File_Index.

      ALI_Index := Get_Index_From_Full_Name
        (ALI_Filename, Insert => False);
      if ALI_Index /= No_Source_File then
         Report
           ("ignoring duplicate ALI file " & ALI_Filename, Kind => Warning);
         return No_Source_File;
      end if;

      ALI_Index := Get_Index_From_Full_Name
        (ALI_Filename, Insert => True);
      Log_File_Open (ALI_Filename);
      Open (ALI_File, In_File, ALI_Filename);

      --  Check that the first line is a valid ALI V line.

      declare
         use Ada.Strings.Unbounded;

         V_Line    : constant String := Get_Stripped_Line (ALI_File);

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
            Line := new String'(Get_Stripped_Line (ALI_File));
            exit when Line'Length > 0;
         end loop;

         case Line (1) is
            when 'A' =>
               if Line.all = "A -fpreserve-control-flow" then
                  Preserve_Control_Flow_Seen := True;

               elsif Line.all = "A -fdump-scos"
                       or else
                     Line.all = "A -gnateS"
               then
                  Dump_SCOs_Seen := True;

               elsif Line.all = "A -g" then
                  Debug_Seen := True;
               end if;

            when 'P' =>
               declare
                  P_Start : Integer;
               begin
                  P_Start := 2;
                  loop
                     while P_Start <= Line'Last
                       and then Line (P_Start) = ' '
                     loop
                        P_Start := P_Start + 1;
                     end loop;
                     exit when P_Start > Line'Last - 1;

                     declare
                        Param : constant String (1 .. 2) :=
                                  Line (P_Start .. P_Start + 1);
                     begin
                        if Param = "NO" then
                           No_Object := True;
                        end if;
                     end;

                     P_Start := P_Start + 2;
                  end loop;
               end;

            when 'U' =>
               Match (U_Matcher, Line (3 .. Line'Last), Matches);
               if Matches (0) /= No_Match then
                  Units.Append (Get_Index_From_Generic_Name (Match (1)));
               end if;

            when 'D' =>
               Match (D_Matcher, Line (3 .. Line'Last), Matches);
               if Matches (0) /= No_Match then
                  Deps.Append (Get_Index_From_Generic_Name
                    (Unquote (Match (1))));
               end if;

            when 'N' =>
               declare
                  Annotation : ALI_Annotation;
                  Valid      : Boolean;
                  Sloc       : Source_Location;
               begin
                  Match (N_Matcher, Line (3 .. Line'Last), Matches);
                  if Matches (0) /= No_Match then
                     declare
                        Note_SFN : constant String := Match (3);
                        Note_SFI : Source_File_Index := Units.Last_Element;

                     begin
                        if Note_SFN'Length > 0 then

                           --  Case of a separate: the source file is not the
                           --  current compilation unit but some other one
                           --  identified explicitly.

                           Note_SFI := Get_Index_From_Generic_Name
                                         (Note_SFN (Note_SFN'First + 1
                                                 .. Note_SFN'Last));
                        end if;

                        Sloc :=
                          (Source_File => Note_SFI,
                           L           => (Line   =>
                                             Integer'Value (Match (1)),
                                           Column =>
                                             Integer'Value (Match (2))));
                     end;

                     Valid := True;

                     begin
                        Annotation :=
                          (CU      => CU,
                           Kind    => ALI_Annotation_Kind'Value (Match (4)),
                           Message => new String'(Match (6)),
                           others  => <>);
                     exception
                        when Constraint_Error =>
                           Report (Sloc, "bad annotation " & Match (4));
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
         if No_Object then
            Put_Line ("warning: no object generated for " & ALI_Filename);

         else
            if not Preserve_Control_Flow_Seen then
               Put_Line
                 ("warning: " & ALI_Filename
                  & ": unit compiled without -fpreserve-control-flow");
            end if;

            if not Dump_SCOs_Seen then
               Put_Line
                 ("warning: " & ALI_Filename
                  & ": unit compiled without SCO generation (-fdump-scos)");
            end if;

            if not Debug_Seen then
               Put_Line
                 ("warning: " & ALI_Filename
                  & ": unit compiled without debug information (-g)");
            end if;
         end if;

         if End_Of_File (ALI_File)
           or else not Dump_SCOs_Seen
           or else No_Object
         then
            --  No SCOs in this ALI

            ALI_Index := No_Source_File;

         else
            Index := 1;
            Get_SCOs_From_ALI;
            Fingerprint :=
              SCOs_Hash (Binary_Message_Digest'(Digest (C_Lines_Ctx)));
         end if;
      end if;

      Close (ALI_File);
      return ALI_Index;
   end Load_ALI;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out ALI_Annotation)
   is
   begin
      CU_Id'Read (S, V.CU);
      ALI_Annotation_Kind'Read (S, V.Kind);
      V.Message := new String'(String'Input (S));
      V.Count := 0;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class; V : ALI_Annotation) is
   begin
      CU_Id'Write (S, V.CU);
      ALI_Annotation_Kind'Write (S, V.Kind);
      String'Output (S, V.Message.all);
   end Write;

end ALI_Files;
