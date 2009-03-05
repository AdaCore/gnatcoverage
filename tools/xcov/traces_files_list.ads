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
with Traces_Files; use Traces_Files;
with Ada.Containers.Doubly_Linked_Lists;
with Strings; use Strings;

--  This package is just a place-holder to contain all traces files.
package Traces_Files_List is
   type Trace_File_Element is record
      Trace : Trace_File_Type;
      Filename : String_Acc;
   end record;

   type Trace_File_Element_Acc is access Trace_File_Element;

   package Traces_Files_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Trace_File_Element_Acc);

   Files : Traces_Files_Lists.List;
end Traces_Files_List;
