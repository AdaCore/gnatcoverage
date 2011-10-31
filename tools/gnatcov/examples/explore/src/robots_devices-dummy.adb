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

with Geomaps; use Geomaps;

package body Robots_Devices.Dummy is

   procedure Update_Square_Ahead (Device : access Dummy_Engine);
   --  Update the square ahead after an engine action

   procedure Fake_Map (Map : in out Geomap);
   --  Build a fake field MAP to perform pretended probes from, with
   --  block borders and a few blocks within.

   procedure Fail (Device : access Dummy_Engine);
   --  Register DEVICE failure

   ----------
   -- Fail --
   ----------

   procedure Fail (Device : access Dummy_Engine) is
   begin
      raise Program_Error;
   end Fail;

   ------------------
   -- Step_Forward --
   ------------------

   procedure Step_Forward (Device : access Dummy_Engine) is
      Sq : Square;
   begin
      Device.Env.Situ.Pos := Pos_Ahead_Of (Device.Env.Situ);
      Sq := Device.Env.Map (Device.Env.Situ.Pos.X, Device.Env.Situ.Pos.Y);

      --  If we steped on Water or into a block, we're destroyed ...
      if Sq = Water or else Sq = Block then
         Fail (Device);
      end if;

      Update_Square_Ahead (Device);
   end Step_Forward;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left (Device : access Dummy_Engine) is
   begin
      if Device.Env.Situ.Dir = Direction'First then
         Device.Env.Situ.Dir := Direction'Last;
      else
         Device.Env.Situ.Dir := Direction'Pred (Device.Env.Situ.Dir);
      end if;

      Update_Square_Ahead (Device);
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   procedure Rotate_Right (Device : access Dummy_Engine) is
   begin
      if Device.Env.Situ.Dir = Direction'Last then
         Device.Env.Situ.Dir := Direction'First;
      else
         Device.Env.Situ.Dir := Direction'Succ (Device.Env.Situ.Dir);
      end if;

      Update_Square_Ahead (Device);
   end Rotate_Right;

   -----------------
   -- Probe_Ahead --
   -----------------

   function Probe_Ahead (Device : access Dummy_Radar) return Square is
   begin
      return Device.Env.Situ.Sqa;
   end Probe_Ahead;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Device : access Dummy_Locator) return Position is
   begin
      return Device.Env.Situ.Pos;
   end Get_Position;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction (Device : access Dummy_Locator) return Direction is
   begin
      return Device.Env.Situ.Dir;
   end Get_Direction;

   ------------------
   -- Get_Hardware --
   ------------------

   function Get_Hardware return Robot_Hardware is
      Hw  : Robot_Hardware;
      Env : Environment_Access := new Environment;
   begin
      --  Create the hardware set.
      Hw := (Eng => new Dummy_Engine,
             Rad => new Dummy_Radar,
             Loc => new Dummy_Locator);

      --  All devices share the same environment.
      Dummy_Engine  (Hw.Eng.all).Env := Env;
      Dummy_Radar   (Hw.Rad.all).Env := Env;
      Dummy_Locator (Hw.Loc.all).Env := Env;

      --  We now initialize the environment with a dummy map and initial
      --  position
      Fake_Map (Env.Map);
      Env.Situ :=
        (Pos => (X => 2, Y => 2),
         Dir => East,
         Sqa => Ground);

      --  Verify that the Sqa value is correct.
      Update_Square_Ahead (Dummy_Engine (Hw.Eng.all)'Access);

      return Hw;
   end Get_Hardware;

   -------------------------
   -- Update_Square_Ahead --
   -------------------------

   procedure Update_Square_Ahead (Device : access Dummy_Engine) is
      Posa : Position renames Pos_Ahead_Of (Device.Env.Situ);
   begin
      Device.Env.Situ.Sqa := Device.Env.Map (Posa.X, Posa.Y);
   end Update_Square_Ahead;

   --------------
   -- Fake_Map --
   --------------

   procedure Fake_Map (Map : in out Geomap) is

      procedure Block_Borders_On (Map : in out Geomap);
      procedure All_Ground_On (Map : in out Geomap);
      procedure Setup_Water_On (Map : in out Geomap);

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

      procedure All_Ground_On (Map : in out Geomap) is
      begin
         for X in Map'Range (Sqx) loop
            for Y in Map'Range (Sqy) loop
               Map (X, Y) := Ground;
            end loop;
         end loop;
      end All_Ground_On;

      procedure Setup_Water_On (Map : in out Geomap) is

         --  Setup 1 pit ~ centered

         Wx : Natural := Map'Length (Sqx);
         Wy : Natural := Map'Length (Sqy);
         Nx : Natural := Wx / 2 + 1;
         Ny : Natural := Wy / 2 + 1;

      begin
         Map (Nx, Ny) := Water;
      end Setup_Water_On;

   begin
      All_Ground_On (Map);
      Block_Borders_On (Map);
      Setup_Water_On (Map);
   end Fake_Map;

end Robots_Devices.Dummy;
