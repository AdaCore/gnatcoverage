------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

package Annotations.Sarif is

   procedure Generate_Report (Context : Coverage.Context_Access);
   --  Generate a report in the SARIF report format. This report format
   --  is designed to be used in visualiztion tools and IDEs, e.g.
   --  VisualStudio. The specification for this format is found here:
   --  https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html
   --
   --  This format is able to represent all gnatcov source coverage metrics and
   --  the corresponding violations. Violations are reported as errors,
   --  undertermined coverage as warnings and exempted violations as notes.

end Annotations.Sarif;
