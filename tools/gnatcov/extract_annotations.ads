------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
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

with Command_Line;

--  Implementation of the "gnatcov extract-annotations" command: turn the
--  in-source annotations of the sources designated by Args (the positional
--  arguments, or the project sources if there is none) into external
--  annotations, and write them, together with the annotations loaded through
--  --external-annotations, to the file passed to --output.
--
--  With --in-place, the in-source annotations are also removed from the
--  sources. Note that the external annotations must then be created against
--  the *rewritten* sources rather than the original ones: the self-relocating
--  Stable_Sloc backends identify a region through a hash of the text of its
--  enclosing declaration, so removing an annotation from a subprogram body
--  invalidates every entry anchored in that body. This unit therefore always
--  rewrites a source before creating any of the entries that designate it.

procedure Extract_Annotations (Args : Command_Line.Parser.Parsed_Arguments);
