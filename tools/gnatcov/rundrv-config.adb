------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2016-2024, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;

with Rundrv.Handlers; use Rundrv.Handlers;

package body Rundrv.Config is

   procedure Get_Builtin_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   --  Helper for Lookup_Driver. If there is a builtin driver available for
   --  the requested target, get a command to run it.

   -------------------------------------------
   -- Matching helpers for built-in targets --
   -------------------------------------------

   type Driver_Creator_Type is
     access procedure
       (Context : Context_Type;
        Matches : Match_Array;
        Cmd     : out Command_Type;
        Native  : out Boolean);
   --  Procedure that creates a command for some target.
   --
   --  Such procedures are evaluated when the target family string matches some
   --  pattern: Matches references the matched pattern. Context contains the
   --  information used to select such a function and Matches contains the
   --  matched substrings in the target family.
   --
   --  The semantics of the Context, Cmd and Native arguments is the same as
   --  for the Lookup_Driver procedure arguments.
   --
   --  Note that the crated command is not expected to contain arguments for
   --  the input program (eargs): the caller will append them at the end of the
   --  arguments list.

   type Builtin_Target_Type is record
      Pattern : Unbounded_String;
      --  Pattern used to match a target family string

      Driver_Creator : Driver_Creator_Type;
      --  Function that creates a command for this target
   end record;
   --  Descriptor for a handled builtin target

   type Builtin_Targets_Type is
     array (Positive range <>) of Builtin_Target_Type;

   Builtin_Targets : constant Builtin_Targets_Type :=
     ((+"(i686|x86_64).*linux", Rundrv.Handlers.Native_Linux'Access),
      (+"(i686|x86_64).*mingw", Rundrv.Handlers.Native_Windows'Access),
      (+"(x86|x86_64)-linux", Rundrv.Handlers.Native_Linux'Access),
      (+"(x86|x86_64)-windows", Rundrv.Handlers.Native_Windows'Access),
      (+"iSystem-(5554|5634)", Rundrv.Handlers.ISystem'Access),
      (+"prepare(32|64)", Rundrv.Handlers.Prepare'Access),
      (+"visium-elf", Rundrv.Handlers.Visium_ELF'Access));
   --  For each target category, this table provides a target triples
   --  (without board name) pattern and a function to create the corresponding
   --  driver.

   ------------------------
   -- Get_Builtin_Driver --
   ------------------------

   procedure Get_Builtin_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean) is
   begin
      for T of Builtin_Targets loop
         declare
            Pattern : constant Pattern_Matcher := Compile (+T.Pattern);
            Matches : Match_Array (0 .. Paren_Count (Pattern));
         begin
            Match (Pattern, Context.Target_Family.all, Matches);
            if Matches (0) /= No_Match then
               Found := True;
               T.Driver_Creator (Context, Matches, Cmd, Native);

               --  Always append the eargs at the end of the command line

               for Earg of Context.Eargs.all loop
                  Append_Arg (Cmd, Earg.all);
               end loop;
               return;
            end if;
         end;
      end loop;

      Found := False;
   end Get_Builtin_Driver;

   -------------------
   -- Lookup_Driver --
   -------------------

   procedure Lookup_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean) is
   begin
      --  If there is a GNATemulator available, just use it

      Get_Gnatemu_Driver (Context, Found, Cmd, Native);
      if Found then
         return;
      end if;

      --  Otherwise, fall back to our knowledge base

      Get_Builtin_Driver (Context, Found, Cmd, Native);
   end Lookup_Driver;

   -----------------------
   -- Available_Targets --
   -----------------------

   function Available_Targets return String is
      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      for T of Builtin_Targets loop
         if not First then
            Append (Result, ", ");
         end if;
         First := False;

         Append (Result, T.Pattern);
      end loop;
      return +Result;
   end Available_Targets;

end Rundrv.Config;
