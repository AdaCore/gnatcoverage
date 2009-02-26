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

with System;

package body Text_IO is

   procedure New_Line is
   begin
      Put (ASCII.CR);
      Put (ASCII.LF);
   end New_Line;

   procedure Put (Item : Character) is
      procedure Internal (C   : Character);
      pragma Import (C, Internal, "putchar");
   begin
      Internal (Item);
   end Put;

   procedure Put (Item : String) is
   begin
      for I in Item'Range loop
         Put (Item (I));
      end loop;
   end Put;

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   procedure Get (Item : out Character) is
      function Internal return Character;
      pragma Import (C, Internal, "getchar");
   begin
      Item := Internal;
   end Get;

   procedure Flush is
   begin
      null;
   end Flush;

end Text_IO;
