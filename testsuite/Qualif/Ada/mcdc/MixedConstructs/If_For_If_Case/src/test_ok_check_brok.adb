
with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_Ok_Check_Brok is
   S : aliased Sensor (Hist_Size => 5);
   
begin
   S.ALB := 5;
   S.AHB := 10;
   
   S.V := 5;
   Sample (S);
   Assert (Status_Of (S) = Ok);
   
   S.V := 1;
   Sample (S);
   Assert (Status_Of (S) = Check);
   
   S.V := 40;
   Sample (S);
   Assert (Status_Of (S) = Broken);
end;

--# sensors-status.adb

-- /SO_t0/       l! ## dT-
-- /SO_undecide/ l- ## s-
-- /SO_loop0/    l+ ## 0
-- /SO_loop1/    l+ ## 0
-- /SO_decide/   l+ ## 0
-- /SO_tfaultLB/ l+ ## 0
-- /SO_tfaultHB/ l+ ## 0
-- /SO_fault/    l+ ## 0
-- /SO_check/    l+ ## 0
-- /SO_broken/   l+ ## 0
-- /SO_PE/       l- ## s-
-- /SO_ret/      l+ ## 0
