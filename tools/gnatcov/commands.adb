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
with Outputs; use Outputs;

package body Commands is

   ------------------
   -- Check_Option --
   ------------------

   procedure Check_Option
     (Option            : String;
      Command           : Command_Type;
      Accepted_Commands : Command_Array) is
   begin
      for J in Accepted_Commands'Range loop
         if Accepted_Commands (J) = Command then
            return;
         end if;
      end loop;

      Fatal_Error ("No option " & Option
                   & " for command " & To_Switch (Command));
   end Check_Option;

   ------------------------
   -- For_Command_Switch --
   ------------------------

   function For_Command_Switch (Command : Command_Type) return String is
   begin
      if Command = No_Command then
         return "";
      else
         return " for " & To_Switch (Command);
      end if;
   end For_Command_Switch;

   ----------------
   -- To_Command --
   ----------------

   function To_Command (Opt_String : String) return Command_Type is
      Literal : String (1 .. Opt_String'Length) := Opt_String;
   begin
      for J in Literal'Range loop
         if Literal (J) = '-' then
            Literal (J) := '_';
         end if;
      end loop;

      begin
         if Literal (1 .. 2) = "__" then
            return Command_Type'Value ("cmd_" & Literal (3 .. Literal'Last));
         else
            return Command_Type'Value ("cmd_" & Literal);
         end if;
      exception
         when Constraint_Error =>
            return No_Command;
      end;
   end To_Command;

   ---------------
   -- To_Switch --
   ---------------

   function To_Switch (Command : Command_Type) return String is
      Result : String := To_Lower (Command'Img);
   begin
      for J in Result'Range loop
         if Result (J) = '_' then
            Result (J) := '-';
         end if;
      end loop;
      --  Skip 'cmd_'.
      return Result (Result'First + 4 .. Result'Last);
   end To_Switch;

end Commands;
