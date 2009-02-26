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

with Links;

procedure Test_Ports is
   package Integer_Links is new Links (Data_Type => Integer);
   use Integer_Links;

   X : Integer;
   IP, OP : aliased Integer_Links.IOport (Capacity => 1);
   L : aliased Integer_Links.IOlink;
begin
   Connect (Outp => OP'Access, Inp => IP'Access, Link => L);

   Push (12, OP);
   Process (L);
   Pop (X, IP);
end Test_Ports;
