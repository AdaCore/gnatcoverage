------------------------------------------------------------------------------
--                                                                          --
--                              GNATcoverage                                --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with Ada.Unchecked_Conversion;

package body Libsupport.Memory is
   
   subtype mem is char_array (size_t);
   type memptr is access all mem;
   function to_memptr is new Ada.Unchecked_Conversion (Address, memptr);

   ------------
   -- memcmp --
   ------------

   function memcmp (S1 : Address; S2 : Address; N : size_t) return int is
      s1_p : constant memptr := to_memptr (S1);
      s2_p : constant memptr := to_memptr (S2);
   begin
      for J in 0 .. N - 1 loop
         if s1_p (J) < s2_p (J) then
            return -1;
         elsif s1_p (J) > s2_p (J) then
            return 1;
         end if;
      end loop;
      return 0;
   end memcmp;

   ------------
   -- memcpy --
   ------------

   procedure memcpy (Dest : Address; Src : Address; N : size_t) is
      dest_p : constant memptr := to_memptr (Dest);
      src_p  : constant memptr := to_memptr (Src);
   begin
      if N > 0 then
         for J in 0 .. N - 1 loop
            dest_p (J) := src_p (J);
         end loop;
      end if;
   end memcpy;

   -------------
   -- memmove --
   -------------

   procedure memmove (Dest : Address; Src : Address; N : size_t) is
      dest_p : constant memptr := to_memptr (Dest);
      src_p  : constant memptr := to_memptr (Src);
   begin
      --  Return immediately if no bytes to copy.

      if N = 0 then
         return;
      end if;

      --  This function must handle overlapping memory regions
      --  for the source and destination. If the Dest buffer is
      --  located past the Src buffer then we use backward copying,
      --  and forward copying otherwise.

      if Dest > Src then
         for J in reverse 0 .. N - 1 loop
            dest_p (J) := src_p (J);
         end loop;
      else
         for J in 0 .. N - 1 loop
            dest_p (J) := src_p (J);
         end loop;
      end if;
   end memmove;

end Libsupport.Memory;
