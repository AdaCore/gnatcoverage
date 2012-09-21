with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_A_FT is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   --  Non empty list. Enter/exit the loop.

   --  First If decision DC covered.
   --  Inner If True only from F or else T.

   Prepend (S'Unchecked_Access, SL);

   S.Active := False;
   Forall_In (SL, Inhibit, Active_Only => True);

   S.Active := True;
   S.ALB := 1;
   S.AHB := 12;
   S.V := 43; -- > high bound
   Forall_In (SL, Inhibit, Active_Only => True);
end;

-- Active  Active_Only  OuterIf   (Action)
-- False   True         False
-- True    True         True      (Inhibit)

--# slists-forall.adb

-- /FA_init/       l+ ## 0
-- /FA_while/      l+ ## 0
-- /FA_tactive/    l! ## c!:"not Active_Only"
-- /FA_case/       l+ ## 0
-- /FA_activate/   l- ## s-
-- /FA_tinhibitLB/ l! ## dF-
-- /FA_tinhibitHB/ l! ## 0
-- /FA_inhibit/    l+ ## 0
-- /FA_next/       l+ ## 0

