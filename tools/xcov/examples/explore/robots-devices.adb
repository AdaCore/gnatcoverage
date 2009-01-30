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

--  This is a fake implementation of the Robots.Devices for the Explore
--  example, allowing simple simulations without real hardware.

package body Robots.Devices is

   --  We construct and maintain an artificial map, force an initial
   --  situation and keep track of it as part of the engine commands
   --  simulation.

   type Hardware is record
      Map  : Geomap;
      Situ : Situation;
   end record;

   procedure Fake_Map (Map : in out Geomap);
   --  Build a fake field MAP to perform pretended probes from, with
   --  block borders and a few blocks within.

   procedure Probe_Ahead (Situ : in out Situation; Map : Geomap);
   --  Fill SITU.Sqa with the square in MAP ahead the location SITU
   --  designates.

   -----------
   -- Probe --
   -----------

   procedure Probe
     (Situ : out Situation; H : Hardware_Access) is
   begin
      --  The current situation is updated by every op execution, so ...
      Situ := H.Situ;
   end Probe;

   -----------------
   -- Probe_Ahead --
   -----------------

   procedure Probe_Ahead (Situ : in out Situation; Map : Geomap) is
      Posa : constant Position := Pos_Ahead_Of (Situ);
   begin
      Situ.Sqa := Map (Posa.X, Posa.Y);
   end Probe_Ahead;

   ------------
   -- Unsafe --
   ------------

   function Unsafe
     (Ctrl : Robot_Control; H : Hardware_Access) return Boolean
   is
   begin
      --  Stepping forward into a rock block or a water pit is unsafe ...

      return Ctrl.Code = Step_Forward
        and then (H.Situ.Sqa = Block or else H.Situ.Sqa = Water);
   end Unsafe;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Ctrl : Robot_Control; H : Hardware_Access) is
   begin
      --  Executing an unsafe command kills ...

      if Unsafe (Ctrl, H) then
         Dump (H.Map, H.Situ);
         raise Program_Error;
      end if;

      --  Otherwise, just pretend a real engine acted as requested
      --  by updating the position/direction accordingly ...

      case Ctrl.Code is
         when Nop => null;

         when Rotate_Left =>
            if H.Situ.Dir = Direction'First then
               H.Situ.Dir := Direction'Last;
            else
               H.Situ.Dir := Direction'Pred (H.Situ.Dir);
            end if;

         when Rotate_Right =>
            if H.Situ.Dir = Direction'Last then
               H.Situ.Dir := Direction'First;
            else
               H.Situ.Dir := Direction'Succ (H.Situ.Dir);
            end if;

         when Step_Forward =>
            H.Situ.Pos := Pos_Ahead_Of (H.Situ);

         when others =>
            raise Program_Error;
      end case;

      --  And keep the full situation up to date by fetching the
      --  new Square ahead.

      Probe_Ahead (H.Situ, H.Map);
   end Execute;

   --------------
   -- Fake_Map --
   --------------

   procedure Fake_Map (Map : in out Geomap) is

      procedure Block_Borders_On (Map : in out Geomap);
      procedure Clear_Playzone_On (Map : in out Geomap);
      procedure Setup_Blocks_On (Map : in out Geomap);

      procedure Block_Borders_On (Map : in out Geomap) is
         X, Y : Natural;
      begin
         X := Map'First (Sqx);
         for Y in Map'Range (Sqy) loop
            Map (X, Y) := Block;
         end loop;
         X := Map'Last (Sqx);
         for Y in Map'Range (Sqy) loop
            Map (X, Y) := Block;
         end loop;

         Y := Map'First (Sqy);
         for X in Map'Range (Sqx) loop
            Map (X, Y) := Block;
         end loop;
         Y := Map'Last (Sqy);
         for X in Map'Range (Sqx) loop
            Map (X, Y) := Block;
         end loop;
      end Block_Borders_On;

      procedure Clear_Playzone_On (Map : in out Geomap) is
      begin
         for X in Map'First (Sqx) + 1 .. Map'Last (Sqx) - 1 loop
            for Y in Map'First (Sqy) + 1 .. Map'Last (Sqy) - 1 loop
               Map (X, Y) := Ground;
            end loop;
         end loop;
      end Clear_Playzone_On;

      procedure Setup_Blocks_On (Map : in out Geomap) is
         X, Y : Natural;

         --  Setup ~ 1 block per five squares in both directions

         Wx : Natural := Map'Length (Sqx);
         Wy : Natural := Map'Length (Sqy);
         Nx : Natural := Wx / 5;
         Ny : Natural := Wy / 5;

         Stepx : Natural := Wx / (Nx + 1);
         Stepy : Natural := Wy / (Ny + 1);
      begin
         Y := Map'First (Sqy) + Stepy;
         while Y <= Map'Last (Sqy) loop
            X := Map'First (Sqx) + Stepx;
            while X <= Map'Last (Sqx) loop
               Map (X, Y) := Block;
               X := X + Stepx;
            end loop;
            Y := Y + Stepy;
         end loop;
      end Setup_Blocks_On;

   begin
      Block_Borders_On (Map);
      Clear_Playzone_On (Map);
      Setup_Blocks_On (Map);
   end Fake_Map;

   ----------
   -- Init --
   ----------

   procedure Init (H : out Hardware_Access) is
   begin
      --  Initialize the pretended devices: build a fake map, force or
      --  initial position to a known-to-be-clear spot, then fill our
      --  current situation Sqa.

      H := new Hardware;

      Fake_Map (H.Map);

      H.Situ.Pos.X := 2;
      H.Situ.Pos.Y := 2;
      H.Situ.Dir := East;

      Probe_Ahead (H.Situ, H.Map);
   end Init;

end Robots.Devices;
