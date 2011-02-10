with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_FU_0 is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  First If decision fully covered.
   --  Inner If decision not reached.

   Prepend (S'Unchecked_Access, SL);
   
   S.Active := False;
   Forall_In (SL, Activate, Active_Only => False);
   
   S.Active := False;
   Forall_In (SL, Inhibit, Active_Only => True);
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l+ 0
-- /FA_case/       l+ 0
-- /FA_activate/   l+ 0
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

