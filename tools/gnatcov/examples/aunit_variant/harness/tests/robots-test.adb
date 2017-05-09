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

with AUnit.Assertions; use AUnit.Assertions;
with Actors;           use Actors;
with Geomaps;          use Geomaps, Geomaps.Situation_Links;
with Controls;         use Controls, Controls.Robot_Control_Links;
with Robots_Devices;   use Robots_Devices;

package body Robots.Test is

   Default_Env : constant Test_Env :=
                   (Robot_Destroyed      => False,
                    Probe_Ahead_Square   => Ground,
                    Position             => (X => 2, Y => 2),
                    Direction            => East);

   ------------------
   -- Step_Forward --
   ------------------

   procedure Step_Forward (Device : access Test_Engine) is
   begin
      case Device.Env.Direction is
         when North =>
            Device.Env.Position.Y := Device.Env.Position.Y - 1;
         when South =>
            Device.Env.Position.Y := Device.Env.Position.Y + 1;
         when West =>
            Device.Env.Position.X := Device.Env.Position.X - 1;
         when East =>
            Device.Env.Position.X := Device.Env.Position.X + 1;
      end case;

      if Device.Env.Probe_Ahead_Square = Water
        or else Device.Env.Probe_Ahead_Square = Block
      then
         Device.Env.Robot_Destroyed := True;
      end if;
   end Step_Forward;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left  (Device : access Test_Engine) is
   begin
      case Device.Env.Direction is
         when North =>
            Device.Env.Direction := West;
         when West =>
            Device.Env.Direction := South;
         when South =>
            Device.Env.Direction := East;
         when East =>
            Device.Env.Direction := North;
      end case;
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   procedure Rotate_Right (Device : access Test_Engine) is
   begin
      case Device.Env.Direction is
         when South =>
            Device.Env.Direction := West;
         when East =>
            Device.Env.Direction := South;
         when North =>
            Device.Env.Direction := East;
         when West =>
            Device.Env.Direction := North;
      end case;
   end Rotate_Right;

   -----------------
   -- Probe_Ahead --
   -----------------

   function Probe_Ahead (Device : access Test_Radar) return Square is
   begin
      return Device.Env.Probe_Ahead_Square;
   end Probe_Ahead;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position  (Device : access Test_Locator) return Position is
   begin
      return Device.Env.Position;
   end Get_Position;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction (Device : access Test_Locator) return Direction is
   begin
      return Device.Env.Direction;
   end Get_Direction;

   -----------
   -- Actor --
   -----------

   function Actor (T : Test) return Actor_Ref is
   begin
      return Actor_Ref (T.Test_Robot);
   end Actor;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test) is
   begin
      if T.Test_Robot = null then
         T.Test_Robot := new Robot;
         T.Hardware :=
           (Eng => new Test_Engine,
            Rad => new Test_Radar,
            Loc => new Test_Locator);
         T.Env := new Test_Env;

         Test_Engine  (T.Hardware.Eng.all).Env := T.Env;
         Test_Radar   (T.Hardware.Rad.all).Env := T.Env;
         Test_Locator (T.Hardware.Loc.all).Env := T.Env;
      end if;

      T.Test_Robot.all :=
        (Actors.Actor with
         Robot_Control_Inp    => null,
         Robot_Situation_Outp => null,
         Hw                   => (Eng => null, Rad => null, Loc => null),
         Mode                 => Cautious);
      T.Env.all := Default_Env;
   end Set_Up;

   ---------------
   -- Test_Init --
   ---------------

   procedure Test_Init (T : in out Test) is
   begin
      Init (T.Test_Robot, T.Hardware);

      Assert
        (T.Test_Robot.Robot_Control_Inp /= null,
         "Robot Control in port not initialized after call to init");

      Assert
        (T.Test_Robot.Robot_Situation_Outp /= null,
         "Robot Control out port not initialized after call to init");
   end Test_Init;

   --------------
   -- Test_Run --
   --------------

   procedure Test_Run (T : in out Test) is
   begin
      Init (T.Test_Robot, T.Hardware);

      --  Test the different controls.

      --  Nop
      T.Env.all := Default_Env;
      Controls.Robot_Control_Links.Push
        (Robot_Control'
           (Code  => Nop,
            Value => 0),
         Robot_Control_Inport (T.Test_Robot.all));
      Run (T.Test_Robot);
      Assert
        (T.Env.all = Default_Env,
         "The robot should not move upon Nop reception");
      Assert
        (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
         "The robot should not send situation report upon Nop reception");

      --  Opmode
      T.Env.all := Default_Env;
      Controls.Robot_Control_Links.Push
        (Robot_Control'
           (Code  => Opmode,
            Value => Robot_Opmode'Pos (Dumb)),
         Robot_Control_Inport (T.Test_Robot.all));
      Run (T.Test_Robot);
      Assert
        (T.Test_Robot.Mode = Dumb,
         "The robot mode should be set to Dumb after (Opmode, Dumb) is sent");
      Assert
        (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
         "The robot should not send situation report upon Opmode reception");

      Controls.Robot_Control_Links.Push
        (Robot_Control'
           (Code  => Opmode,
            Value => Robot_Opmode'Pos (Cautious)),
         Robot_Control_Inport (T.Test_Robot.all));
      Run (T.Test_Robot);
      Assert
        (T.Test_Robot.Mode = Cautious,
         "The robot mode should be set to Cautions after (Opmode, Cautious)" &
         " is sent");
      Assert
        (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
         "The robot should not send situation report upon Opmode reception");

      --  Step_Forward
      --  Several cases should be tested here:
      --  1- The square ahead is neither Water nor block, then we move forward
      --     whatever the mode.
      --  2- The square ahead is Water or Block, then we don't move forward in
      --     Cautious mode.
      --  3- The square ahead is Water or Block, then we move forward anyway
      --     in Dumb mode

      --  Test Step_Forward 1: ground ahead, mode = cautious or dumb
      for M in Robot_Opmode'Range loop

         T.Env.all := Default_Env;
         T.Test_Robot.Mode := M;
         T.Env.Probe_Ahead_Square := Ground;
         T.Env.Position := (X => 2, Y => 2);
         T.Env.Direction := South;

         Controls.Robot_Control_Links.Push
           (Robot_Control'
              (Code  => Step_Forward,
               Value => 0),
            Robot_Control_Inport (T.Test_Robot.all));
         Run (T.Test_Robot);

         Assert
           (T.Env.Position = (X => 2, Y => 3),
            "The robot should have stepped forward upon Step_Forward with" &
            " ground ahead");
         Assert
           (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
            "The robot should not send situation report upon Step forward" &
            " reception");
      end loop;

      --  Test Step_Forward 2: Water/Block ahead, mode = cautious
      for Sq in Block .. Water loop

         T.Env.all := Default_Env;
         T.Test_Robot.Mode := Cautious;
         T.Env.Probe_Ahead_Square := Sq;
         T.Env.Position := (X => 2, Y => 2);
         T.Env.Direction := South;

         Controls.Robot_Control_Links.Push
           (Robot_Control'
              (Code  => Step_Forward,
               Value => 0),
            Robot_Control_Inport (T.Test_Robot.all));
         Run (T.Test_Robot);

         Assert
           (not T.Env.Robot_Destroyed
            and then T.Env.Position = (X => 2, Y => 2),
            "The robot should not have stepped forward upon Step_Forward " &
            "with water or block ahead (Cautious mode)");
      end loop;

      --  Test Step_Forward 3: Water/Block ahead, mode = dumb
      for Sq in Block .. Water loop

         T.Env.all := Default_Env;
         T.Test_Robot.Mode := Dumb;
         T.Env.Probe_Ahead_Square := Sq;
         T.Env.Position := (X => 2, Y => 2);
         T.Env.Direction := South;

         Controls.Robot_Control_Links.Push
           (Robot_Control'
              (Code  => Step_Forward,
               Value => 0),
            Robot_Control_Inport (T.Test_Robot.all));
         Run (T.Test_Robot);

         Assert
           (T.Env.Robot_Destroyed,
            "The robot should have stepped forward upon Step_Forward " &
            "with water or block ahead (Dumb mode)");
         Assert
           (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
            "The robot should not send situation report upon Step forward" &
            " reception");
      end loop;

      --  Turn_Right, Turn_Left
      --  We should be able to turn right/left unconditionally.
      for Sq in Ground .. Water loop
         for M in Robot_Opmode'Range loop
            T.Env.all := Default_Env;
            T.Test_Robot.Mode := M;
            T.Env.Probe_Ahead_Square := Sq;
            T.Env.Position := (X => 2, Y => 2);
            T.Env.Direction := South;

            Controls.Robot_Control_Links.Push
              (Robot_Control'
                 (Code  => Rotate_Right,
                  Value => 0),
               Robot_Control_Inport (T.Test_Robot.all));
            Run (T.Test_Robot);
            Assert
              (T.Env.Direction = West,
               "The robot should have turned right upon Rotate_Right cmd");

            Controls.Robot_Control_Links.Push
              (Robot_Control'
                 (Code  => Rotate_Left,
                  Value => 0),
               Robot_Control_Inport (T.Test_Robot.all));
            Run (T.Test_Robot);
            Assert
              (T.Env.Direction = South,
               "The robot should have turned left upon Rotate_Left cmd");
            Assert
              (Empty (Robot_Situation_Outport (T.Test_Robot.all)),
               "The robot should not send situation report upon Step forward" &
               " reception");
         end loop;
      end loop;

      --  Probe
      for Sq in Ground .. Water loop
         for Dir in Direction'Range loop
            declare
               The_Env : Test_Env;
               Situ    : Situation;

            begin
               The_Env := Default_Env;
               The_Env.Probe_Ahead_Square := Sq;
               The_Env.Position := (X => 2, Y => 2);
               The_Env.Direction := Dir;
               T.Env.all := The_Env;

               Controls.Robot_Control_Links.Push
                 (Robot_Control'
                    (Code  => Probe,
                     Value => 0),
                  Robot_Control_Inport (T.Test_Robot.all));
               Run (T.Test_Robot);

               Assert
                 (T.Env.all = The_Env,
                  "The robot should not have moved upon probe");
               Assert
                 (not Empty (Robot_Situation_Outport (T.Test_Robot.all)),
                  "The robot should have sent a situation upon probe");

               Pop (Situ, Robot_Situation_Outport (T.Test_Robot.all));
               Assert
                 (Situ = (Pos => The_Env.Position,
                          Dir => Dir,
                          Sqa => Sq),
                  "Unexpected Situation received after a probe");
            end;
         end loop;
      end loop;
   end Test_Run;

end Robots.Test;
