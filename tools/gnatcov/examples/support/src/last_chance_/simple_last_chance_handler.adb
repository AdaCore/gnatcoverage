------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with GNAT.IO;

package body Simple_Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_Abort;
      pragma Import (C, C_Abort, "abort");
      pragma No_Return (C_Abort);

      procedure Lch_Enter;
      pragma Import (Ada, Lch_Enter, "__lch_enter");
   begin
      Lch_Enter;

      GNAT.IO.New_Line;
      GNAT.IO.Put_Line ("!!!!!!!!!!!!!!!!!!!!!!!!");
      GNAT.IO.Put_Line ("!!! EXCEPTION RAISED !!!");
      GNAT.IO.Put_Line ("!!!!!!!!!!!!!!!!!!!!!!!!");

      --  No return procedure.
      C_Abort;
   end Last_Chance_Handler;

end Simple_Last_Chance_Handler;
