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

with GNAT.Strings; use GNAT.Strings;

function Project_Find_Source_File (Simple_Name : String) return String_Access;
--  Look for the absolute path for the source file called Simple_Name in the
--  loaded project tree. If no such source file is found, return null.
--  Otherwise, a string is allocated and returned. In this case, the caller
--  is responsible for deallocating the returned access.
--
--  If no project is loaded, just return null.

--  Keeping this function separated from the other Project APIs is necessary to
--  avoid including GPR2 in links that do not really need it.
