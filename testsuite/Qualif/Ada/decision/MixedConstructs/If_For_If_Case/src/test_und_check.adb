
with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_Und_Check is
   S : aliased Sensor (Hist_Size => 5);
   
begin
   S.ALB := 5;
   S.AHB := 10;
   
   Assert (Status_Of (S) = Undecidable);
   
   S.V := 1;
   Sample (S);
   Assert (Status_Of (S) = Check);
   
   S.V := 5;
   Sample (S);
   Assert (Status_Of (S) = Check);
   
end;

--# sensors-status.adb

-- /SO_t0/       l+ 0
-- /SO_undecide/ l+ 0
-- /SO_loop0/    l+ 0
-- /SO_loop1/    l+ 0
-- /SO_decide/   l+ 0
-- /SO_tfaultLB/ l+ 0
-- /SO_tfaultHB/ l+ 0
-- /SO_fault/    l+ 0
-- /SO_check/    l+ 0
-- /SO_broken/   l- s-
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
