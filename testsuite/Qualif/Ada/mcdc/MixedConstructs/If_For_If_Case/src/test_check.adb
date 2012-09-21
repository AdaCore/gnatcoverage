with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_Check is
   S : aliased Sensor (Hist_Size => 5);

begin
   S.ALB := 1;
   S.AHB := 10;

   --  Single history entry

   S.V := 15; -- > High bound
   Sample (S);

   Assert (Status_Of (S) = Check);
   -- LB-check  HB-check  Failure
   -- False     True      False

   --  Extra history entry, in-range

   S.V := 5;
   Sample (S);

   --  Overall, two history entries, only one out of range

   Assert (Status_Of (S) = Check);
   -- LB-check  HB-check  Failure
   -- False     True      False
   -- False     False     True
end;

--# sensors-status.adb

-- /SO_t0/       l! ## dT-
-- /SO_undecide/ l- ## s-
-- /SO_decide/   l+ ## 0
-- /SO_loop0/    l+ ## 0
-- /SO_loop1/    l+ ## 0
-- /SO_tfaultLB/ l! ## c!:"S.Hist"
-- /SO_tfaultHB/ l! ## 0
-- /SO_fault/    l+ ## 0
-- /SO_check/    l+ ## 0
-- /SO_broken/   l- ## s-
-- /SO_PE/       l- ## s-
-- /SO_ret/      l+ ## 0
