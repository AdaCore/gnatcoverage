with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_A_FF is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  First If decision DC covered.
   --  Inner If False only from F or else F.

   Prepend (S'Unchecked_Access, SL);

   S.Active := False;
   Forall_In (SL, Inhibit, Active_Only => True);

   S.Active := True;
   S.ALB := 1;
   S.AHB := 12;
   S.V := 3; -- in range
   Forall_In (SL, Inhibit, Active_Only => True);
end;

-- Active  Active_Only  OuterIf   (Action)
-- False   True         False
-- True    True         True      (Inhibit)

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l! c!:"not Active_Only"
-- /FA_case/       l+ 0
-- /FA_activate/   l- s-
-- /FA_tinhibitLB/ l! dT-
-- /FA_tinhibitHB/ l! 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

