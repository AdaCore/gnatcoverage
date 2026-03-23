with Support, Plop; use Support, Plop;
procedure Test_TF is
begin
   Assert (Steps_To_LT (X => 5.0, LB => 1.0, Max => 10) = 5);
end;

--# plop.adb
--  /eval/ l+ ## 0
