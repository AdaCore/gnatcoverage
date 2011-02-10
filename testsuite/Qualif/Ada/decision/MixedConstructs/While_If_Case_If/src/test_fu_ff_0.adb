with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_FF_0 is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  Inactive sensor only, loop over active only => don't even reach the
   --  inner If decision.

   Prepend (S'Unchecked_Access, SL);

   Forall_In (SL, Inhibit, Active_Only => True);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l! dT-
-- /FA_case/       l- s-
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

