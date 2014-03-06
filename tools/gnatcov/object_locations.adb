------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with Interfaces;

with GNAT.Regpat;  use GNAT.Regpat;
with GNAT.Strings; use GNAT.Strings;

with Diagnostics; use Diagnostics;
with Files_Table;
with Hex_Images;  use Hex_Images;
with Outputs;     use Outputs;
with Switches;    use Switches;
with Symbols;
with Types;       use Types;

package body Object_Locations is

   use type Pc_Type;

   -----------------------------------------------------
   --  Regular expressions for user locations parsing --
   -----------------------------------------------------

   Sloc_Range_RE     : constant Pattern_Matcher :=
     Compile ("^(.*):(\d+):(\d+)-(\d+):(\d+)$");
   Around_Address_RE : constant Pattern_Matcher :=
     Compile ("^@0x([0-9a-fA-F]+)$");
   Address_Range_RE  : constant Pattern_Matcher :=
     Compile ("^0x([0-9a-fA-F]+)..0x([0-9a-fA-F]+)$");

   function Get_Symbol
     (Exec : Exe_File_Type;
      Name : String) return Address_Info_Acc;
   --  Return the symbol that matches Name, or null if there is no such symbol.
   --  Performs a linear search to do so.

   function Hex_Value (S : String) return Pc_Type;
   --  Parse an hexadecimal string (without 0x prefix)

   -------------------------
   -- Parse_User_Location --
   -------------------------

   function Parse_User_Location (S : String) return User_Location is
      Matches : Match_Array (0 .. 5);

      function Match (Index : Integer) return String is
        (S (Matches (Index).First .. Matches (Index).Last));
      --  Return the substring of S corresponding to the Index regexp match

      File_Index : Source_File_Index;
   begin
      Match (Sloc_Range_RE, S, Matches);
      if Matches (0) /= No_Match then
         File_Index := Files_Table.Get_Index_From_Simple_Name
           (Match (1), Insert => True);
         return
           (Kind       => Sloc_Range,
            Sloc_Range => To_Range
              ((Source_File => File_Index,
                L           => (Integer'Value (Match (2)),
                                Integer'Value (Match (3)))),
               (Source_File => File_Index,
                L           => (Integer'Value (Match (4)),
                                Integer'Value (Match (5))))));
      end if;

      Match (Around_Address_RE, S, Matches);
      if Matches (0) /= No_Match then
         return
           (Kind    => Around_Address,
            Address => Hex_Value (Match (1)));
      end if;

      Match (Address_Range_RE, S, Matches);
      if Matches (0) /= No_Match then
         return
           (Kind     => Address_Range,
            PC_First => Hex_Value (Match (1)),
            PC_Last  => Hex_Value (Match (2)));
      end if;

      return (Kind => Symbol,
              Name => Symbols.To_Symbol (S));
   end Parse_User_Location;

   ------------------------
   -- Translate_Location --
   ------------------------

   function Translate_Location
     (Exec : Exe_File_Acc; User_Loc : User_Location) return Proc_Location
   is
   begin
      case User_Loc.Kind is
         when Sloc_Range =>
            return
              (Kind       => Sloc_Range,
               Sloc_Range => User_Loc.Sloc_Range);

         when Around_Address =>
            declare
               Symbol : constant Address_Info_Acc :=
                 Get_Symbol (Exec.all, User_Loc.Address);
            begin
               if Symbol = null then
                  Warn ("No symbol at " & Hex_Image (User_Loc.Address));
                  return No_Proc_Location;

               else
                  return
                    (Kind     => Address_Range,
                     PC_First => Symbol.First,
                     PC_Last  => Symbol.Last);
               end if;
            end;

         when Address_Range =>
            return
              (Kind     => Address_Range,
               PC_First => User_Loc.PC_First,
               PC_Last  => User_Loc.PC_Last);

         when Symbol =>
            declare
               Name   : constant String :=
                 Symbols.To_String (User_Loc.Name).all;
               Symbol : constant Address_Info_Acc :=
                 Get_Symbol (Exec.all, Name);
            begin
               if Symbol = null then
                  Warn ("No such symbol: " & Name);
                  return No_Proc_Location;

               else
                  return (Kind     => Address_Range,
                          PC_First => Symbol.First,
                          PC_Last  => Symbol.Last);
               end if;
            end;
      end case;
   end Translate_Location;

   -------------------------
   -- Translate_Locations --
   -------------------------

   procedure Translate_Locations
     (Exec      : Exe_File_Acc;
      User_Locs : User_Locations;
      Proc_Locs : in out Proc_Locations) is
   begin
      for User_Loc of User_Locs loop
         declare
            Loc : constant Proc_Location :=
              Translate_Location (Exec, User_Loc);
         begin
            if Loc /= No_Proc_Location then
               Proc_Locs.Append (Loc);
               if Switches.Verbose then
                  Report
                    (Msg  =>
                       ("Location: " & Image (Loc)
                        & " (from " & Image (User_Loc) & ")"),
                     Kind => Notice);
               end if;

            elsif Switches.Verbose then
               Report
                 (Msg  => "Ignore location: " & Image (User_Loc),
                  Kind => Notice);
            end if;
         end;
      end loop;
   end Translate_Locations;

   -----------------------
   -- Matches_Locations --
   -----------------------

   function Matches_Locations
     (Exec      : Exe_File_Acc;
      Locations : Proc_Locations;
      PC        : Pc_Type) return Boolean
   is
      Result : Boolean := False;
      S      : Address_Info_Acc;

      function "<=" (L, R : Local_Source_Location) return Boolean is
        (L.Line < R.Line
         or else (L.Line = R.Line and then L.Column <= R.Column));
      --  Return if L is before R. Unlike in the Slocs package,
      --  No_Sloc_Location must sort lower than specific slocs.

      function In_Sloc_Range
        (Sloc : Source_Location;
         Sloc_Range : Source_Location_Range) return Boolean is
        (Sloc.Source_File = Sloc_Range.Source_File
         and then Sloc_Range.L.First_Sloc <= Sloc.L
         and then Sloc.L <= Sloc_Range.L.Last_Sloc);
      --  Return if Sloc belongs to in Sloc_Range

   begin
      for Loc of Locations loop
         case Loc.Kind is
            when Address_Range =>
               Result := PC in Loc.PC_First .. Loc.PC_Last;
            when Sloc_Range =>
               S := Get_Address_Info
                 (Exec.all, Subprogram_Addresses, PC);
               if S /= null then
                  declare
                     Sloc : constant Address_Info_Acc :=
                       Get_Address_Info (S.Lines, Line_Addresses, PC);
                  begin
                     Result :=
                       (Sloc /= null
                        and then In_Sloc_Range (Sloc.Sloc, Loc.Sloc_Range));
                  end;
               end if;
         end case;
         if Result then
            return True;
         end if;
      end loop;
      return False;
   end Matches_Locations;

   -----------
   -- Image --
   -----------

   function Image (L : User_Location) return String is
   begin
      case L.Kind is
         when Sloc_Range =>
            return "Sloc range " & Image (L.Sloc_Range);
         when Around_Address =>
            return "Around address " & Hex_Image (L.Address);
         when Address_Range =>
            return
              ("Address range " & Hex_Image (L.PC_First)
               & ".." & Hex_Image (L.PC_Last));
         when Symbol =>
            return "Symbol " & Symbols.To_String (L.Name).all;
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (L : Proc_Location) return String is
   begin
      case L.Kind is
         when Address_Range =>
            return
              ("Address range " & Hex_Image (L.PC_First)
               & ".." & Hex_Image (L.PC_Last));
         when Sloc_Range =>
            return "Sloc range " & Image (L.Sloc_Range);
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (L : Address_Info_Acc) return String is
      use Interfaces;

      Discr_Suffix : constant String :=
        (if L.Disc > 0
         then " discriminator" & Unsigned_32'Image (L.Disc)
         else "");
   begin
      return Image (L.Sloc) & Discr_Suffix;
   end Image;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Exec : Exe_File_Type;
      Name : String) return Address_Info_Acc
   is
      Cur    : Addresses_Iterator;
      Symbol : Address_Info_Acc;
   begin
      Init_Iterator (Exec, Symbol_Addresses, Cur);
      loop
         Next_Iterator (Cur, Symbol);
         exit when Symbol = null;

         if Symbol.Symbol_Name /= null
           and then Symbol.Symbol_Name.all = Name
         then
            return Symbol;
         end if;
      end loop;
      return null;
   end Get_Symbol;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (S : String) return Pc_Type is
   begin
      return Pc_Type'Value ("16#" & S & "#");
   end Hex_Value;

end Object_Locations;
