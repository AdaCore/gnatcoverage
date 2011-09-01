with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_Undecide is
   S : aliased Sensor (Hist_Size => 5);
   
begin
   S.ALB := 1;
   S.AHB := 10;
   S.V := 5;
   
   Assert (Status_Of (S) = Undecidable);
end;

--# sensors-status.adb

-- /SO_t0/       l+ 0
-- /SO_undecide/ l+ 0
-- /SO_loop0/    l- s-
-- /SO_loop1/    l- 0c
-- /SO_decide/   l- s-
-- /SO_tfaultLB/ l- s-
-- /SO_tfaultHB/ l- 0c
-- /SO_fault/    l- s-
-- /SO_check/    l- s-
-- /SO_broken/   l- s-
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
