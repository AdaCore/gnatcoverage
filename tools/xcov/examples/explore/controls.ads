--------------------------------------------------------------------------
--                             CONTROLS (SPEC)                          --
--------------------------------------------------------------------------

--  This unit exposes the Robot_Control enumeral, commands the robot
--  understands, and instanciates the associated Link abstraction.

with Links;

package Controls is

   --  Possible Robot Operational modes

   type Robot_Opmode is
     (Cautious,  --  The Robot won't execute potentially damaging commands
      Dumb       --  The Robot executes all the commands it receives
     );

   --  Commands the robot understands on its "robot control" input port.

   type Robot_Command is
     (Nop,           --  Do nothing
      Opmode,        --  Switch to new operational mode
      Step_Forward,  --  Step forward
      Rotate_Right,  --  Rotate 90 deg, counterclockwise
      Rotate_Left,   --  Rotate 90 deg, clockwise
      Probe          --  Probe field one step ahead and report
     );

   type Robot_Control is record
      Code  : Robot_Command;
      Value : Integer; --  Possible argument associated with command CODE.
   end record;

   package Robot_Control_Links is new Links (Data_Type => Robot_Control);
end;
