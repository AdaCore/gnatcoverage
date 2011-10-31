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

package body Robots is

   --------------------
   -- IOports access --
   --------------------

   function Robot_Control_Inport
     (R : Robot) return Robot_Control_Links.IOport_Access is
   begin
      return R.Robot_Control_Inp;
   end Robot_Control_Inport;

   function Robot_Situation_Outport
     (R : Robot) return Situation_Links.IOport_Access is
   begin
      return R.Robot_Situation_Outp;
   end Robot_Situation_Outport;

   ----------------------
   -- Input processing --
   ----------------------

   use Robot_Control_Links;

   procedure Process_Next_Control (Port : Robot_Control_Links.IOport_Access);
   --  Process the next control command available from PORT

   ------------
   -- Unsafe --
   ------------

   function Unsafe (Cmd : Robot_Command; Sqa : Square) return Boolean is
   begin
      --  Stepping forward with a rock block or a water pit ahead is Unsafe

      return
        Cmd = Step_Forward
        and then (Sqa = Block or else Sqa = Water);
   end Unsafe;

   --------------------------
   -- Process_Next_Control --
   --------------------------

   procedure Process_Next_Control
     (Port : Robot_Control_Links.IOport_Access)
   is
      Ctrl  : Robot_Control;
      Robot : Robot_Access := Robot_Access (Owner (Port));

   begin
      Pop (Ctrl, Port);

      --  In Cautious mode, the robot refuses to process unsafe controls

      if Robot.Mode = Cautious
        and then Unsafe (Ctrl.Code, Probe_Ahead (Robot.Hw.Rad))
      then
         return;
      end if;

      case Ctrl.Code is
         when Nop =>
            return;

         when Opmode =>
            Robot.Mode := Robot_Opmode'Val (Ctrl.Value);

         when Step_Forward =>
            Step_Forward (Robot.Hw.Eng);

         when Rotate_Left =>
            Rotate_Left (Robot.Hw.Eng);

         when Rotate_Right =>
            Rotate_Right (Robot.Hw.Eng);

         when Probe =>
            Situation_Links.Push
              (Situation'
                 (Pos => Get_Position (Robot.Hw.Loc),
                  Dir => Get_Direction (Robot.Hw.Loc),
                  Sqa => Probe_Ahead (Robot.Hw.Rad)),
               Robot_Situation_Outport (Robot.all));

      end case;
   end Process_Next_Control;

   ---------
   -- Run --
   ---------

   procedure Run (R : Robot_Access) is
      Control_Port : IOport_Access := Robot_Control_Inport (R.all);
   begin
      while not Empty (Control_Port) loop
         Process_Next_Control (Control_Port);
      end loop;
   end Run;

   ----------
   -- Init --
   ----------

   procedure Init (R : Robot_Access; Hw : Robot_Hardware) is
   begin
      R.Robot_Situation_Outp :=
        Situation_Links.Create_IOport
         (Capacity => 1,
          Owner => Actor_Ref (R));
      R.Robot_Control_Inp :=
        Robot_Control_Links.Create_IOport
         (Capacity => 2,
          Owner    => Actor_Ref (R));
      R.Hw := Hw;
   end Init;

end Robots;
