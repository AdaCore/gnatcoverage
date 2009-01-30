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

with Robots.Devices;

package body Robots is

   type Robot_Hardware is record
      DH : Devices.Hardware_Access;
   end record;

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

   procedure Process_Action (Ctrl : Robot_Control; R : Robot_Access);
   --  Have robot R process command CTRL, requiring device action

   procedure Process_Probe (R : Robot_Access);
   --  Have robot R process a Probe request

   procedure Process_Next_Control (Port : Robot_Control_Links.IOport_Access);
   --  Process the next control command available from PORT

   --------------------
   -- Process_Action --
   --------------------

   procedure Process_Action (Ctrl : Robot_Control; R : Robot_Access) is
   begin
      if R.Mode = Cautious and then Devices.Unsafe (Ctrl, R.H.DH) then
         return;
      else
         Devices.Execute (Ctrl, R.H.DH);
      end if;
   end Process_Action;

   -------------------
   -- Process_Probe --
   -------------------

   procedure Process_Probe (R : Robot_Access) is
      Situ : Situation;
   begin
      Devices.Probe (Situ, R.H.DH);
      Situation_Links.Push (Situ, Robot_Situation_Outport (R.all));
   end Process_Probe;

   --------------------------
   -- Process_Next_Control --
   --------------------------

   procedure Process_Next_Control
     (Port : Robot_Control_Links.IOport_Access)
   is
      Ctrl : Robot_Control;
      Robot : Robot_Access := Robot_Access (Port.Owner);
   begin
      Pop (Ctrl, Port);

      case Ctrl.Code is
         when Nop =>
            null;
         when Opmode =>
            Robot.Mode := Robot_Opmode'Val (Ctrl.Value);
         when Step_Forward | Rotate_Left | Rotate_Right =>
            Process_Action (Ctrl, Robot);
         when Probe =>
            Process_Probe (Robot);
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

   procedure Init (R : Robot_Access) is
   begin
      R.Robot_Situation_Outp
        := new Situation_Links.IOport (Capacity => 1,
                                       Owner => Actor_Ref (R));
      R.Robot_Control_Inp
        := new Robot_Control_Links.IOport (Capacity => 2,
                                           Owner => Actor_Ref (R));

      --  Emitters of Probe commands expect being able to fetch the
      --  Situation right after the request is sent, so we make sure
      --  that at least these ones are processed immediately.

      --  On_Push (R.Robot_Control_Inp, Process_One_Control'Access);

      R.H := new Robot_Hardware;
      Devices.Init (R.H.DH);
   end Init;

end Robots;
