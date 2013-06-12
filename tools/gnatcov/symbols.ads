------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

--  Few handy utilities when using symbol tables

with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GNATCOLL.Utils;   use GNATCOLL.Utils;

package Symbols is

   function "<" (S1, S2 : Symbol) return Boolean;
   --  Return wheter S1 < S2. Enables one to use symbols as keys for ordered
   --  sets/maps.

   function To_Symbol (S : String) return Symbol;
   --  Turn a name to a symbol using a unique symbol table

   function To_String (S : Symbol) return Cst_String_Access;
   --  Get a string back from a symbol

end Symbols;
