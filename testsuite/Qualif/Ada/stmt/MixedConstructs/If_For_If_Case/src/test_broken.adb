with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_Broken is
   S : aliased Sensor (Hist_Size => 5);
   
begin
   S.ALB := 1;
   S.AHB := 10;
   
   --  Two history entries, both out of range
   
   S.V := 15;   
   Sample (S);
   Sample (S);
   
   Assert (Status_Of (S) = Broken);
      
   --  A third entry, in range. Still two out of range.
   
   S.V := 5;   
   Sample (S);
   
   Assert (Status_Of (S) = Broken);
end;

-- Note that we get into the Check state internally, before
-- moving to Broken.

--# sensors-status.adb

-- /SO_t0/       l+ 0
-- /SO_undecide/ l- s-
-- /SO_decide/   l+ 0
-- /SO_loop0/    l+ 0
-- /SO_loop1/    l+ 0
-- /SO_tfaultLB/ l+ 0
-- /SO_tfaultHB/ l+ 0
-- /SO_fault/    l+ 0
-- /SO_check/    l+ 0
-- /SO_broken/   l+ 0
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
