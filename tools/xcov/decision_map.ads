------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  Management of the decision map

--  The decision map provides information on conditional branches in object
--  code that correspond to source level decisions. It is used to establish
--  decision coverage (DC) and modified condition/decision coverage (MC/DC)
--  coverage properties.

with Strings; use Strings;

package Decision_Map is

   procedure Analyze (ALI_List_Filename : String_Acc);
   --  Build the decision map from the executable, debug information and
   --  the Source Coverage Obligations.

   procedure Write_Map (Filename : String);
   --  Write the contents of the decision map to the named file

end Decision_Map;
