------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Ada.Text_IO; use Ada.Text_IO;

package body Display is
   type Vt100_Color_Patterns is array (Color) of String (1 .. 7);

   Vt100_Colors : constant Vt100_Color_Patterns :=
     (
      Green   => ASCII.ESC & "[1;32m",
      Red     => ASCII.ESC & "[1;31m",
      Black   => ASCII.ESC & "[0;30m",
      Cyan    => ASCII.ESC & "[1;36m",
      Magenta => ASCII.ESC & "[1;35m"
     );

   Current_Color : Color := Black;

   procedure Set_Color (C : Color) is
   begin
      if Flag_Color and C /= Current_Color then
         Put (Vt100_Colors (C));
         Current_Color := C;
      end if;
   end Set_Color;
end Display;
