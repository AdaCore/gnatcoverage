with Actuators, Support; use Actuators, Support;

procedure Test_Fast is
   A : Actuator;
begin
   A.Value := 0;
   A.Safety := 2;
   Fast_Increment (A, 5);
   Assert (A.Value = 5);
end;

--# actuators.adb
--  /check/  l+ ## 0
--  /punt/   l- ## s-
--  /update/ l+ ## 0
--  /call/   l+ ## 0
