with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_A_A is
   SL  : Sensor_List;
   S1, S2 : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  First If decision DC covered.
   --  Inner If True only from T or else X.

   Prepend (S1'Unchecked_Access, SL);
   Prepend (S2'Unchecked_Access, SL);

   S1.Active := False;
   S2.Active := False;
   Forall_In (SL, Inhibit, Active_Only => True);

   S1.Active := True;
   S1.ALB := 1;
   S1.AHB := 12;
   S1.V := 0; -- < low bound

   S2.Active := True;
   S2.ALB := 1;
   S2.AHB := 12;
   S2.V := 4; -- in range
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
-- /FA_tinhibitLB/ l! 0
-- /FA_tinhibitHB/ l! c!:"Na.S.V"
-- /FA_inhibit/    l+ 0
-- /FA_next/       l+ 0

