with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_F_0_0 is
   SL  : Sensor_List;
begin
   --  Empty list, only. Never entering the loop.  The conditions inside the
   --  loop depend on the list items, so there's no sense at all in trying to
   --  set anything else.

   Forall_In (SL, Activate);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l! d!
-- /FA_tactive/    l- s-
-- /FA_case/       l- s-
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l- s-

