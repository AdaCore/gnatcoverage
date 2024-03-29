with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_B_0 is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  First If decision DC covered.
   --  Inner If decision not reached.

   Prepend (S'Unchecked_Access, SL);

   S.Active := False;
   Forall_In (SL, Activate, Active_Only => False);

   S.Active := False;
   Forall_In (SL, Inhibit, Active_Only => True);
end;

-- Active  Active_Only  OuterIf   (Action)
-- False   False        True      (Activate)
-- False   True         False

--# slists-forall.adb

-- /FA_init/       l+ ## 0
-- /FA_while/      l+ ## 0
-- /FA_tactive/    l! ## c!:"Na.S.Active"
-- /FA_case/       l+ ## 0
-- /FA_activate/   l+ ## 0
-- /FA_tinhibitLB/ l- ## s-
-- /FA_tinhibitHB/ l- ## 0c
-- /FA_inhibit/    l- ## s-
-- /FA_next/       l+ ## 0
