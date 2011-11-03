------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ S C O S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with SCOs;  use SCOs;
with Types; use Types;

with Ada.IO_Exceptions; use Ada.IO_Exceptions;

procedure Get_SCOs is
   Dnum : Nat;
   C    : Character;
   Loc1 : Source_Location;
   Loc2 : Source_Location;
   Cond : Character;
   Dtyp : Character;

   use ASCII;
   --  For CR/LF

   function At_EOL return Boolean;
   --  Skips any spaces, then checks if we are the end of a line. If so,
   --  returns True (but does not skip over the EOL sequence). If not,
   --  then returns False.

   procedure Check (C : Character);
   --  Checks that file is positioned at given character, and if so skips past
   --  it, If not, raises Data_Error.

   function Get_Int return Int;
   --  On entry the file is positioned to a digit. On return, the file is
   --  positioned past the last digit, and the returned result is the decimal
   --  value read. Data_Error is raised for overflow (value greater than
   --  Int'Last), or if the initial character is not a digit.

   procedure Get_Source_Location (Loc : out Source_Location);
   --  Reads a source location in the form line:col and places the source
   --  location in Loc. Raises Data_Error if the format does not match this
   --  requirement. Note that initial spaces are not skipped.

   procedure Get_Source_Location_Range (Loc1, Loc2 : out Source_Location);
   --  Skips initial spaces, then reads a source location range in the form
   --  line:col-line:col and places the two source locations in Loc1 and Loc2.
   --  Raises Data_Error if format does not match this requirement.

   procedure Skip_EOL;
   --  Called with the current character about to be read being LF or CR. Skips
   --  past CR/LF characters until either a non-CR/LF character is found, or
   --  the end of file is encountered.

   procedure Skip_Spaces;
   --  Skips zero or more spaces at the current position, leaving the file
   --  positioned at the first non-blank character (or Types.EOF).

   ------------
   -- At_EOL --
   ------------

   function At_EOL return Boolean is
   begin
      Skip_Spaces;
      return Nextc = CR or else Nextc = LF;
   end At_EOL;

   -----------
   -- Check --
   -----------

   procedure Check (C : Character) is
   begin
      if Nextc = C then
         Skipc;
      else
         raise Data_Error;
      end if;
   end Check;

   -------------
   -- Get_Int --
   -------------

   function Get_Int return Int is
      Val : Int;
      C   : Character;

   begin
      C := Nextc;
      Val := 0;

      if C not in '0' .. '9' then
         raise Data_Error;
      end if;

      --  Loop to read digits of integer value

      loop
         declare
            pragma Unsuppress (Overflow_Check);
         begin
            Val := Val * 10 + (Character'Pos (C) - Character'Pos ('0'));
         end;

         Skipc;
         C := Nextc;

         exit when C not in '0' .. '9';
      end loop;

      return Val;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Get_Int;

   -------------------------
   -- Get_Source_Location --
   -------------------------

   procedure Get_Source_Location (Loc : out Source_Location) is
      pragma Unsuppress (Range_Check);
   begin
      Loc.Line := Logical_Line_Number (Get_Int);
      Check (':');
      Loc.Col := Column_Number (Get_Int);
   exception
      when Constraint_Error =>
         raise Data_Error;
   end Get_Source_Location;

   -------------------------------
   -- Get_Source_Location_Range --
   -------------------------------

   procedure Get_Source_Location_Range (Loc1, Loc2 : out Source_Location) is
   begin
      Skip_Spaces;
      Get_Source_Location (Loc1);
      Check ('-');
      Get_Source_Location (Loc2);
   end Get_Source_Location_Range;
   --------------
   -- Skip_EOL --
   --------------

   procedure Skip_EOL is
      C : Character;

   begin
      loop
         Skipc;
         C := Nextc;
         exit when C /= LF and then C /= CR;

         if C = ' ' then
            Skip_Spaces;
            C := Nextc;
            exit when C /= LF and then C /= CR;
         end if;
      end loop;
   end Skip_EOL;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      while Nextc = ' ' loop
         Skipc;
      end loop;
   end Skip_Spaces;

--  Start of processing for Get_Scos

begin
   SCOs.Initialize;

   --  Loop through lines of SCO information

   while Nextc = 'C' loop
      Skipc;

      C := Getc;

      --  Make sure first line is a header line

      if SCO_Unit_Table.Last = 0 and then C /= ' ' then
         raise Data_Error;
      end if;

      --  Otherwise dispatch on type of line

      case C is

         --  Header entry

         when ' ' =>

            --  Complete previous entry if any

            if SCO_Unit_Table.Last /= 0 then
               SCO_Unit_Table.Table (SCO_Unit_Table.Last).To :=
                 SCO_Table.Last;
            end if;

            --  Scan out dependency number and file name

            declare
               Ptr : String_Ptr := new String (1 .. 32768);
               N   : Integer;

            begin
               Skip_Spaces;
               Dnum := Get_Int;

               Skip_Spaces;

               N := 0;
               while Nextc > ' ' loop
                  N := N + 1;
                  Ptr.all (N) := Getc;
               end loop;

               --  Make new unit table entry (will fill in To later)

               SCO_Unit_Table.Append (
                 (File_Name => new String'(Ptr.all (1 .. N)),
                  Dep_Num   => Dnum,
                  From      => SCO_Table.Last + 1,
                  To        => 0));

               Free (Ptr);
            end;

         --  Statement entry

         when 'S' | 's' =>
            declare
               Typ : Character;
               Key : Character;

            begin
               --  If continuation, reset Last indication in last entry
               --  stored for previous CS or cs line, and start with key
               --  set to s for continuations.

               if C = 's' then
                  SCO_Table.Table (SCO_Table.Last).Last := False;
                  Key := 's';

               --  CS case (first line, so start with key set to S)

               else
                  Key := 'S';
               end if;

               --  Initialize to scan items on one line

               Skip_Spaces;

               --  Loop through items on one line

               loop
                  Typ := Nextc;

                  if Typ in '1' .. '9' then
                     Typ := ' ';
                  else
                     Skipc;
                  end if;

                  Get_Source_Location_Range (Loc1, Loc2);

                  Add_SCO
                    (C1   => Key,
                     C2   => Typ,
                     From => Loc1,
                     To   => Loc2,
                     Last => At_EOL);

                  exit when At_EOL;
                  Key := 's';
               end loop;
            end;

         --  Decision entry

         when 'I' | 'E' | 'P' | 'W' | 'X' =>
            Dtyp := C;
            Skip_Spaces;

            --  Output header

            declare
               Loc : Source_Location;
               C2v : Character;

            begin
               --  Acquire location information

               if Dtyp = 'X' then
                  Loc := No_Source_Location;
               else
                  Get_Source_Location (Loc);
               end if;

               --  C2 is a space except for pragmas where it is 'e' since
               --  clearly the pragma is enabled if it was written out.

               if C = 'P' then
                  C2v := 'e';
               else
                  C2v := ' ';
               end if;

               Add_SCO
                 (C1   => Dtyp,
                  C2   => C2v,
                  From => Loc,
                  To   => No_Source_Location,
                  Last => False);
            end;

            --  Loop through terms in complex expression

            C := Nextc;
            while C /= CR and then C /= LF loop
               if C = 'c' or else C = 't' or else C = 'f' then
                  Cond := C;
                  Skipc;
                  Get_Source_Location_Range (Loc1, Loc2);
                  Add_SCO
                    (C2   => Cond,
                     From => Loc1,
                     To   => Loc2,
                     Last => False);

               elsif C = '!' or else
                     C = '&' or else
                     C = '|'
               then
                  Skipc;

                  declare
                     Loc : Source_Location;
                  begin
                     Get_Source_Location (Loc);
                     Add_SCO (C1 => C, From => Loc, Last => False);
                  end;

               elsif C = ' ' then
                  Skip_Spaces;

               else
                  raise Data_Error;
               end if;

               C := Nextc;
            end loop;

            --  Reset Last indication to True for last entry

            SCO_Table.Table (SCO_Table.Last).Last := True;

         --  No other SCO lines are possible

         when others =>
            raise Data_Error;
      end case;

      Skip_EOL;
   end loop;

   --  Here with all SCO's stored, complete last SCO Unit table entry

   SCO_Unit_Table.Table (SCO_Unit_Table.Last).To := SCO_Table.Last;
end Get_SCOs;
