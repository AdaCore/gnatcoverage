with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_Inhibit is
   S1, S2, S3 : aliased Sensor (Hist_Size => 5);
   SL  : Sensor_List;
begin
   S1.ALB := 1;
   S1.AHB := 10;
   S1.V   := 5;  -- # in range
   S1.Active := True;
   
   S2.ALB := 5;
   S2.AHB := 15;
   S2.V   := 1;  -- # < low bound
   S2.Active := True;

   S3.ALB := 5;
   S3.AHB := 15;
   S3.V   := 45;  -- # > high bound
   S3.Active := True;

   Prepend (S1'Unchecked_Access, SL);
   Prepend (S2'Unchecked_Access, SL);
   Prepend (S3'Unchecked_Access, SL);
   
   Forall_In (SL, Inhibit);
   Assert (S1.Active and then (not S2.Active) and then (not S3.Active));
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l+ 0
-- /FA_case/       l+ 0
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l+ 0
-- /FA_tinhibitHB/ l+ 0
-- /FA_inhibit/    l+ 0
-- /FA_next/       l+ 0

