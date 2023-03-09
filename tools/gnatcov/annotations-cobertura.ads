------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Coverage;

package Annotations.Cobertura is

   --  Generate a report in the Cobertura report format. This report format
   --  is designed for integration with standard continuous integration
   --  systems, e.g. gitlab CI, which do not support standard gnatcov report
   --  formats. The only specification for this format is a document type
   --  definition found there:
   --  https://github.com/cobertura/web/blob/master/htdocs/xml/coverage-04.dtd
   --
   --  This is a partial report in the sense that gnatcov coverage metrics
   --  can't be specified accurately in the Cobertura report format. This
   --  format provides
   --
   --     * Line coverage metrics: whether a line was covered, and the number
   --       of coverage hits.
   --
   --     * Condition coverage metrics: whether a line is branching (talking
   --       Cobertura terminology, which means whether it has any condition).
   --       For each condition, whether its true / false / both valuation(s)
   --       were covered.
   --
   --  Obviously, neither decision and MC/DC coverage can be expressed using
   --  the aforementioned metrics without little tweaks. We actually chose
   --  to represent decision coverage in the condition coverage metrics:
   --  a decision (gnatcov) is represented as a condition (cobertura). We drop
   --  the MC/DC coverage as we can't express it.
   --
   --  Note that for now, at least some of the continuous integration systems
   --  (this is the case for gitlab at least) do not integrate condition
   --  coverage results, which means that users won't have decision coverage
   --  results over their code if they use it with gitlab.

   procedure Generate_Report (Context : Coverage.Context_Access);

   function Installed return Boolean;
   --  Return whether the DTD is installed, i.e. the required resource files
   --  are installed in lib/gnatcoverage/.

end Annotations.Cobertura;
