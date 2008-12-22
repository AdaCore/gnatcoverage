------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Dummy implementation.

with System.Storage_Elements;
with Unchecked_Conversion;

package body Memory is

   package SSE renames System.Storage_Elements;

   Default_Size : constant := 10 * 1_024;

   type Mark_Id is new SSE.Integer_Address;

   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;

   Mem : Memory (1 .. Default_Size);

   Top : Mark_Id := Mem'First;

   function To_Mark_Id is new Unchecked_Conversion
     (size_t, Mark_Id);

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : Mark_Id :=
                       ((To_Mark_Id (Size) + Max_Align - 1) / Max_Align)
                       * Max_Align;
      Location     : constant Mark_Id := Top;
   begin
      if Max_Size = 0 then
         Max_Size := Max_Align;
      end if;

      if Size = size_t'Last then
         raise Storage_Error;
      end if;

      Top := Top + Max_Size;

      if Top > Default_Size then
         raise Storage_Error;
      end if;

      return Mem (Location)'Address;
   end Alloc;

end Memory;
