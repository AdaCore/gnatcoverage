------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

--  This package is used for multiple kinds of reporting, and uses a somewhat
--  generic message structure to fill that purpose. It is used to:
--
--  * Display verbose messages
--
--  * Report warnings / errors during execution
--
--  * Report coverage analysis results (e.g. coverage violation messages)
--
--  Every message emitted that is attached to a line (i.e. for which a sloc is
--  reported, most notably: coverage violation messages) is stored into gnatcov
--  internal tables, and is (in most cases) not displayed by report procedures,
--  but rather included during coverage report production.
--
--  Other messages (most warnings, errors) are simply displayed on the standard
--  output at the time the report procedure is called.

with Ada.Containers.Vectors;

with Logging;
with SC_Obligations; use SC_Obligations;
with Slocs;          use Slocs;
with Strings;        use Strings;
with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;

package Diagnostics is

   Diagnostics_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("DIAGNOSTICS");

   type Report_Kind is
     (Notice,
      Low_Warning,
      Warning,
      Error,
      Info,
      Violation,
      Undetermined_Cov,
      Exclusion);
   subtype Coverage_Kind is Report_Kind range Info .. Exclusion;

   type Message is record
      Kind           : Report_Kind;
      Exe            : Exe_File_Acc;
      PC             : Pc_Type;
      Sloc           : Source_Location;
      Violation_Sloc : Source_Location;
      SCO            : SCO_Id;
      Msg            : Unbounded_String;
   end record;

   package Message_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Message);

   procedure Report
     (Exe  : Exe_File_Acc;
      PC   : Pc_Type;
      Msg  : String;
      SCO  : SCO_Id := No_SCO_Id;
      Kind : Report_Kind := Error);

   procedure Report
     (Sloc : Source_Location; Msg : String; Kind : Report_Kind := Error);

   procedure Report_Coverage
     (SCO : SCO_Id; Msg : String; Kind : Coverage_Kind);
   --  Report coverage messages. Common processing for Report_Violation and
   --  Report_Exclusion.

   procedure Report_Violation (SCO : SCO_Id; Msg : String);
   --  Report a violation of a source coverage obligation. Note: the SCO kind
   --  will be prepended to Msg in reports, unless Msg starts with ^ (caret).
   --  A violation message has message kind Violation.

   procedure Report_Exclusion (SCO : SCO_Id; Msg : String);
   --  Report exclusion of a SCO from coverage analysis. No coverage status
   --  will be reported for SCO. Note: the SCO kind will be prepended to Msg in
   --  reports, unless Msg starts with ^ (caret). A violation message has
   --  message kind Exclusion.

   procedure Report
     (Msg            : String;
      Exe            : Exe_File_Acc := null;
      PC             : Pc_Type := No_PC;
      Sloc           : Source_Location := No_Location;
      Violation_Sloc : Source_Location := No_Location;
      SCO            : SCO_Id := No_SCO_Id;
      Kind           : Report_Kind := Error);
   --  Output diagnostic message during coverage analysis. Messages with Notice
   --  kind are omitted unless global flag Verbose is set. A prefix is
   --  prepended depending on message kind:
   --     --- notice
   --     *** warning
   --     !!! error
   --     .C. coverage info
   --     !C! coverage violation
   --     -C- coverage exclusion
   --  The message is also recorded in the source line information for its sloc
   --  or in the Detached_Messages vector, if there is no such source line
   --  information. If SCO is not No_SCO_Id, the message denotes a violation
   --  of the denoted Source Coverage Obligation.

   function Image (M : Message) return String;

end Diagnostics;
