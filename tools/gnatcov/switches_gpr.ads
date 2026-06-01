------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2026, AdaCore                     --
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

--  Extensions to the Switches package for APIs that rely on GPR2.
--
--  Keeping them separated from the other Switches APIs is necessary to avoid
--  including GPR2 in links that do not really need it.

with GPR2;

with Switches; use Switches;

package Switches_GPR is

   procedure Parse_Arguments (From_Driver : Boolean);
   --  Load arguments from command-line and from the project file (if any) into
   --  Switches.Args. Print usage and exit if there is no argument.
   --
   --  If From_Driver is True, do not compute the lists of projects/units of
   --  interest from project files. This is meant to be used only in the
   --  gnatcov driver, where we just need to determine the target.

   function To_Language (Id : GPR2.Language_Id) return Some_Language;
   --  Convert a GPR2 lanugage ID to our enumeration value. Abort with a fatal
   --  error if Name is invalid.

   function To_Language_Or_All (Id : GPR2.Language_Id) return Any_Language;
   --  Like To_Language, but return All_Languages if Name is invalid

   function To_Language_Id (Language : Some_Language) return GPR2.Language_Id;
   --   Convert our enumeration value for languages to the corresponding GPR2
   --   language ID.

end Switches_GPR;
