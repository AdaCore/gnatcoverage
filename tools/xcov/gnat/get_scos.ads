------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ S C O S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines used to deal with generation and output
--  of Soure Coverage Obligations (SCO's) used for coverage analysis purposes.

with SCOs;  use SCOs;
with Types; use Types;
generic
   with function Getc return Character;
   with function Nextc return Character;
function Get_SCOS (Info : Int := 0) return Unit_Index;
--  Load SCO information for given unit into SCOs tables. Getc gets the next
--  character from the ALI file, consuming it. It returns 16#1A# to mark the
--  end of file. Nextc looks at the next character without consuming it. The
--  procedure is called with Getc ready to read the C in the first column of
--  the first line of the SCO information for a unit in the ALI file. Reading
--  stops on encountering either the end of file, or a non-blank line starting
--  with a character other than C. On return, Getc would read either this end
--  of file character or the character other than C. Info is simply some kind
--  of identifying information which is stashed in the SCO_Unit_Table for later
--  retrieval (e.g. in the compiler it is the Unit_Number_Type value). If it
--  is not needed, it can be ommitted and left as zero. The returned value is
--  the index of the new entry created in the SCO_Unit_Table.
