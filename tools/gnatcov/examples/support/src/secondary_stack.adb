------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2010, AdaCore                       --
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

--  Avoid the warning for using an internal unit.
pragma Warnings (Off);
with System.Secondary_Stack;
pragma Warnings (On);

with System.Storage_Elements;

package body Secondary_Stack is

   package SSS renames System.Secondary_Stack;
   package SSE renames System.Storage_Elements;

   Default_Size : constant := 10 * 1_024;

   Chunk : array (1 .. Default_Size) of
      SSE.Storage_Element;

   Uninitialized : Boolean := True;

   function Get_Sec_Stack return System.Address is
   begin
      if Uninitialized then
         Uninitialized := False;
         SSS.SS_Init (Chunk'Address, Default_Size);
      end if;
      return Chunk'Address;
   end Get_Sec_Stack;

end Secondary_Stack;
