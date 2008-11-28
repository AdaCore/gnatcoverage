--------------------------------------------------------------------------
--                             CONTROLS (SPEC)                          --
--------------------------------------------------------------------------

--  This unit exposes the Robot_Control enumeral, commands the robot
--  understands, and instanciates the associated Link abstraction.

with Links;

package Controls is

   --  Commands the robot understands on its "robot control" input port.

   type Robot_Control is
     (Nop,           --  Do nothing
      Step_Forward,  --  Step forward
      Rotate_Right,  --  Rotate 90 deg, counterclockwise
      Rotate_Left,   --  Rotate 90 deg, clockwise
      Probe          --  Probe field one step ahead and report
     );

   package Robot_Control_Links is new Links (Data_Type => Robot_Control);
end;
