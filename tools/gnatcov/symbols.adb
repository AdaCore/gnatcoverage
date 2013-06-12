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

with System;      use System;

package body Symbols is

   Strings : constant Symbol_Table_Access := Allocate;
   --  String table, used both to reduce memory consumption and to make string
   --  comparison very efficient. Since this table is going to be used during
   --  the whole execution of gnatcov, it won't be free'd.

   ---------
   -- "<" --
   ---------

   function "<" (S1, S2 : Symbol) return Boolean is
   begin
      return Get (S1).all'Address < Get (S2).all'Address;
   end "<";

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol (S : String) return Symbol
   is
   begin
      return Strings.Find (S);
   end To_Symbol;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : Symbol) return Cst_String_Access
   is
   begin
      return Get (S);
   end To_String;

end Symbols;
