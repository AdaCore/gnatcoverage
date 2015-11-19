------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

package body Command_Line is

   -------------------
   -- Bool_Callback --
   -------------------

   procedure Bool_Callback
     (Result : in out Parsed_Arguments;
      Option : Bool_Options)
   is
   begin
      case Option is
         when Opt_Include =>
            Result.Remaining_Args.Append (+"--include");
         when Opt_Exclude =>
            Result.Remaining_Args.Append (+"--exclude");
         when others =>
            null;
      end case;
   end Bool_Callback;

   --------------------------
   -- String_List_Callback --
   --------------------------

   procedure String_List_Callback
     (Result : in out Parsed_Arguments;
      Option : String_List_Options;
      Value  : String)
   is
   begin
      case Option is
         when Opt_Exec =>
            declare
               Item : constant Unbounded_String := +(ASCII.NUL & Value);
            begin
               Result.String_List_Args (Opt_Trace).Append (Item);
            end;

         when others =>
            null;
      end case;
   end String_List_Callback;

   ------------------
   -- Arg_Callback --
   ------------------

   procedure Arg_Callback
     (Result : in out Parsed_Arguments;
      Value  : String)
   is
   begin
      case Result.Command is
         when Cmd_Coverage
            | Cmd_Dump_Trace
            | Cmd_Dump_Trace_Raw
            | Cmd_Dump_Trace_Base =>

            --  For these commands, remaining arguments are trace files:
            --  transfer them to Opt_Trace to unify traces handling.

            Result.String_List_Args (Opt_Trace).Append (+Value);

         when Cmd_Dump_Trace_Asm =>

            --  This one is particular: the first argument is an executable
            --  file and the other ones are trace files.

            if Result.String_List_Args (Opt_Exec).Is_Empty then
               Result.String_List_Args (Opt_Exec).Append (+Value);
            else
               Result.String_List_Args (Opt_Trace).Append (+Value);
            end if;

         when others =>
            null;
      end case;
   end Arg_Callback;

end Command_Line;
