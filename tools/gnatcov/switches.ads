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

package Switches is

   Verbose : Boolean := False;
   --  Verbose informational output

   All_Decisions : Boolean := False;
   --  If True, perform decision coverage in stmt+decision mode even for
   --  decisions outside of control structures.

   All_Messages : Boolean := False;
   --  If True, then when performing source coverage analysis, also include in
   --  the report messages other than violations of a source coverage
   --  obligation.

   ------------------------------
   -- Debugging switches (-d?) --
   ------------------------------

   Debug_Full_History : Boolean := False;
   --  -dh
   --  Keep full historical traces for MC/DC even for decisions that do not
   --  require it (decisions without diamond paths).

   Debug_Ignore_Exemptions : Boolean := False;
   --  -di
   --  Exemption pragmas have no effect.

end Switches;
