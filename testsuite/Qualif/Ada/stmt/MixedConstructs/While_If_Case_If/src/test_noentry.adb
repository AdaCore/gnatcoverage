with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_Noentry is
   SL  : Sensor_List;
begin
   Forall_In (SL, Activate);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l- s-
-- /FA_case/       l- s-
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l- s-

