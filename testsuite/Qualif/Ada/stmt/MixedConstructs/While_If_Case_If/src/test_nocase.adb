with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_Nocase is
   S : aliased Sensor (Hist_Size => 5);
   SL  : Sensor_List;
begin
   S.ALB := 1;
   S.AHB := 10;
   S.Active := False;
   Prepend (S'Unchecked_Access, Sl);
   Forall_In (SL, Inhibit, Active_Only => True);
   Assert (not S.Active);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l+ 0
-- /FA_case/       l- s-
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0c
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

