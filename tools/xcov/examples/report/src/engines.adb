------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2011, AdaCore                    --
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

package body Engines is

   -----------------
   -- May_Speedup --
   -----------------

   function May_Speedup
     (E : Engine; By : Natural) return Boolean;
   --  Whether bumping E.Speed by BY is allowed, when either E should
   --  accept anything or the increment would leave it stable

   function May_Speedup
     (E : Engine; By : Natural) return Boolean is
   begin
      return E.Mode = Dumb or else E.Speed + By <= E.Tolerance;
   end May_Speedup;

   -------------
   -- Process --
   -------------

   procedure Process
     (Cmd : Command; E : in out Engine) is
   begin
      if Cmd.Kind = Stop then
         E.Speed := 0;
      elsif Cmd.Kind = Accelerate
        and then May_Speedup (E => E, By => Cmd.Increment)
      then
         E.Speed := E.Speed + Cmd.Increment;
      end if;
   end Process;

   ------------
   -- Stable --
   ------------

   function Stable
     (E : Engine) return Boolean is
   begin
      return E.Speed <= E.Tolerance;
   end Stable;

   -----------
   -- Check --
   -----------

   procedure Check
     (E : in out Engine) is
   begin
      if not Stable (E) and then E.Mode = Safe then
         Process (Cmd => (Kind => Stop), E => E);
      end if;
   end check;

end Engines;
