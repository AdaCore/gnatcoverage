----------------------------------------------------------------------------
--                             ROBOTS (BODY)                              --
----------------------------------------------------------------------------

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
   end;

   function Robot_Situation_Outport
     (R : Robot) return Situation_Links.IOport_Access is
   begin
      return R.Robot_Situation_Outp;
   end;

   ----------------------
   -- Input processing --
   ----------------------

   use Robot_Control_Links;

   procedure Process_Action (R : Robot_Access; Ctrl : Robot_Control) is
   begin
      --  Should check that the command is safe here.  Blind deferral
      --  to the devices may lead to crash.

      Devices.Execute (Ctrl, R.H.DH);
   end;

   procedure Process_Probe (R : Robot_Access) is
      Situ : Situation;
   begin
      Devices.Probe (Situ, R.H.DH);
      Situation_Links.Push (Situ, Robot_Situation_Outport (R.all));
   end;

   procedure Process_One_Control
     (Port : Robot_Control_Links.IOport_Access)
   is
      Ctrl : Robot_Control;
      Robot : Robot_Access := Robot_Access (Port.Owner);
   begin
      Pop (Ctrl, Port);

      case Ctrl is
         when Nop | Step_Forward | Rotate_Left | Rotate_Right =>
            Process_Action (Robot, Ctrl);
         when Probe =>
            Process_Probe (Robot);
      end case;
   end;

   ---------
   -- Run --
   ---------

   procedure Run (R : Robot_Access) is
      Control_Port : IOport_Access := Robot_Control_Inport (R.all);
   begin
      while not Empty (Control_Port) loop
         Process_One_Control (Control_Port);
      end loop;
   end;

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
   end;

end;
