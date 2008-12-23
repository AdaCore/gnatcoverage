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

   function Unsafe (Ctrl : Robot_Control; R : Robot_Access) return Boolean;
   --  Whether execution of CTRL by Robot R is unsafe

   procedure Process_Action (Ctrl : Robot_Control; R : Robot_Access);
   --  Have R process command CTRL, requiring device action

   procedure Process_Probe (R : Robot_Access);
   --  Have robot R process a Probe request

   procedure Process_Next_Control (Port : Robot_Control_Links.IOport_Access);
   --  Process the next control command available from PORT

   ------------
   -- Unsafe --
   ------------

   function Unsafe
     (Ctrl : Robot_Control; R : Robot_Access) return Boolean
   is
      Situ : Situation;
   begin
      Devices.Probe (Situ, R.H.DH);

      --  Given the current situation in SITU, evaluate evaluate
      --  if the CTRL command is unsafe.  Start by assuming it is
      --  not and adjust.

      declare
         Is_Unsafe : Boolean := False;
      begin
         --  Stepping forward with a rock block on the square ahead
         --  would crash the robot on the block

         if Ctrl.Code = Step_Forward and then Situ.Sqa = Block then
            Is_Unsafe := True;
         end if;

         return Is_Unsafe;
      end;
   end;

   --------------------
   -- Process_Action --
   --------------------

   procedure Process_Action (Ctrl : Robot_Control; R : Robot_Access) is
   begin
      if R.Mode = Cautious and then Unsafe (Ctrl, R) then
         return;
      else
         Devices.Execute (Ctrl, R.H.DH);
      end if;
   end;

   -------------------
   -- Process_Probe --
   -------------------

   procedure Process_Probe (R : Robot_Access) is
      Situ : Situation;
   begin
      Devices.Probe (Situ, R.H.DH);
      Situation_Links.Push (Situ, Robot_Situation_Outport (R.all));
   end;

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
   end;

   ---------
   -- Run --
   ---------

   procedure Run (R : Robot_Access) is
      Control_Port : IOport_Access := Robot_Control_Inport (R.all);
   begin
      while not Empty (Control_Port) loop
         Process_Next_Control (Control_Port);
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
