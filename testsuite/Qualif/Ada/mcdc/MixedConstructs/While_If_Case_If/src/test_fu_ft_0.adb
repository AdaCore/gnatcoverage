with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_FT_0 is
   SL  : Sensor_List;
   S : aliased Sensor;
begin
   -- While decision covered from loop over non empty list
   -- First If decision True only from F or else T. Inner If not reached.

   S.Active := False;
   S.ALB := 1;
   S.AHB := 15;
   S.V := 30;
   Prepend (S'Unchecked_Access, SL);

   Forall_In (SL, Activate, Active_Only => False);
end;

--# slists-forall.adb

-- /FA_init/       l+ ## 0
-- /FA_while/      l+ ## 0
-- /FA_tactive/    l! ## dF-
-- /FA_case/       l+ ## 0
-- /FA_activate/   l+ ## 0
-- /FA_tinhibitLB/ l- ## s-
-- /FA_tinhibitHB/ l- ## 0c
-- /FA_inhibit/    l- ## s-
-- /FA_next/       l+ ## 0

