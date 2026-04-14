------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

package GPR2.Build.Actions.Instrument_Source.Ada is

   type Object is new GPR2.Build.Actions.Instrument_Source.Object with private;

   overriding
   procedure Compute_Signature
     (Self            : in out Object;
      Signature       : in out GPR2.Build.Signature.Object;
      Check_Checksums : Boolean);

   overriding
   function Dependencies (Self : in out Object) return Containers.Filename_Set;
   --  Retrieve the dependencies of the unit instrumented in the context of
   --  the given source instrumentation action.

   overriding
   procedure Write_Instrumented_Files_List (Self : in out Object);

private

   type Object is new GPR2.Build.Actions.Instrument_Source.Object
   with null record;

end GPR2.Build.Actions.Instrument_Source.Ada;
