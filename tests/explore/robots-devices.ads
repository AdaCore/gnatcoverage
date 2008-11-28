----------------------------------------------------------------------------
--                           ROBOTS.DEVICES (SPEC)                        --
----------------------------------------------------------------------------

--  This unit offers a simple device access internal API to the Robot
--  abstraction of the Explore example.
--
--  This API abstracts the capabilities of
--
--  - a steering engine, able to have the robot perform a short step
--    forward or rotate 90 deg either way without advancing,
--
--  - a front radar, able to probe the field characteristics one step ahead,
--
--  - a locator, able to evaluate the robot's position and orientation on
--    the field.

with Controls, Geomaps;
use Controls, Geomaps;

private package Robots.Devices is

   type Hardware_Access is private;

   procedure Init (H : out Hardware_Access);
   --  Initialize a Robot devices structure and have H designate it

   procedure Probe (Situ : out Situation; H : Hardware_Access);
   --  Use the devices to fill SITU with the current Situation

   procedure Execute (Ctrl : Robot_Control; H : Hardware_Access);
   --  Use the devices to execute CTRL

private

   --  There will be fake implementations of this unit for pure software
   --  simulation purposes, so we defer the type definition to the package
   --  body.

   type Hardware;
   type Hardware_Access is access Hardware;
end;
