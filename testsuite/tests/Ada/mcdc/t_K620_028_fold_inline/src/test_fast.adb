pragma Ada_2005;

with Actuators, Support; use Actuators, Support;

procedure Test_Fast is

   A : Actuator := (Value => 0, Safety => 2);
begin
   Fast_Increment (A, 5);
   Assert (A.Value = 5);
end;

--# actuators.adb
--  /check/  l! ## dT-
--  /punt/   l- ## s-
--  /update/ l+ ## 0
--  /call/   l+ ## 0
