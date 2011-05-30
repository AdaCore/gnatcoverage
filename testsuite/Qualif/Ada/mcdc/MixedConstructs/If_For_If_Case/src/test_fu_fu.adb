with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_FU_FU is
   S : Sensor;

begin
   -- Outer and Inner If both True and False.

   S.ALB := 1;
   S.AHB := 15;

   Assert (Status_Of (S) = Undecidable);

   S.V := 0; -- < low bound
   Sample (S);


   S.V := 5; -- in range
   Sample (S);
   Assert (Status_Of (S) = Check);

   -- LB-check  HB-check  Failure
   -- True      False     True
   -- False     False     False
end;

--# sensors-status.adb

-- /SO_t0/       l+ 0
-- /SO_undecide/ l+ 0
-- /SO_decide/   l+ 0
-- /SO_loop0/    l+ 0
-- /SO_loop1/    l+ 0
-- /SO_tfaultLB/ l! 0
-- /SO_tfaultHB/ l! c!:"S.Hist"
-- /SO_fault/    l+ 0
-- /SO_check/    l+ 0
-- /SO_broken/   l- s-
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
