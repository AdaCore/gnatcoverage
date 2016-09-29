------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Strings; use Strings;

package body Switches is

   Debug_Switches_Help : constant array (Valid_Debug_Type)
                                  of Ada.Strings.Unbounded.Unbounded_String :=
     (Break_Long_Instructions =>
         +"Break long instructions in disassemblies, a la objdump",
      Full_History =>
         +("Keep full historical traces for MC/DC even for decisions that do"
           & " not require it (decisions without diamond paths)."),
      Ignore_Exemptions =>
         +"Exemption pragmas have no effect.",
      File_Table =>
         +"Print debugging messages for files table management.");

   --------------------------------
   -- Debug_Command_Line_Pattern --
   --------------------------------

   function Debug_Command_Line_Pattern return String is
      Result : Unbounded_String;
   begin
      for C in Debug_Switches_Map'Range loop
         if Debug_Switches_Map (C) /= None then
            if Length (Result) >= 1 then
               Append (Result, '|');
            end if;
            Append (Result, C);
         end if;
      end loop;
      return To_String (Result);
   end Debug_Command_Line_Pattern;

   -----------------------------
   -- Debug_Command_Line_Help --
   -----------------------------

   function Debug_Command_Line_Help return String is
      Result : Unbounded_String;
   begin
      Append (Result, "Debug switch: change various behaviors.");
      for C in Debug_Switches_Map'Range loop
         declare
            S : constant Debug_Type := Debug_Switches_Map (C);
         begin
            if S /= None then
               Append (Result, ASCII.LF & ASCII.LF);
               Append (Result, "  -d" & C & ": " & Debug_Switches_Help (S));
            end if;
         end;
      end loop;
      return To_String (Result);
   end Debug_Command_Line_Help;

end Switches;
