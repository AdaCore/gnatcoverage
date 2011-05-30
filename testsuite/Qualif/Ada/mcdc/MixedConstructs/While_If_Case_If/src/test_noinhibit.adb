with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_Noinhibit is
   S1 : aliased Sensor (Hist_Size => 5);
   SL  : Sensor_List;
begin
   -- get into the inhibit case selection, arranging for the
   -- if statement there to always evaluate false
   
   S1.ALB := 1;
   S1.AHB := 10;
   S1.V   := 5;  -- # in range
   S1.Active := True;
   
   Prepend (S1'Unchecked_Access, SL);
   
   Forall_In (SL, Inhibit);
   Assert (S1.Active);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l! dF-
-- /FA_case/       l+ 0
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l! dT-
-- /FA_tinhibitHB/ l! 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

