with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_TX_0 is
   SL : Sensor_List;
   S1 : aliased Sensor;
begin
   -- While decision covered from loop over non empty list
   -- First If decision True only from T or else X. Inner If not reached.

   S1.Active := True;
   S1.ALB := 1;
   S1.AHB := 15;
   S1.V := 30;
   Prepend (S1'Unchecked_Access, SL);

   for Aonly in False .. True loop
      Forall_In (SL, Activate, Active_Only => Aonly);
   end loop;
end;

--# slists-forall.adb

-- /FA_init/       l+ 0
-- /FA_while/      l+ 0
-- /FA_tactive/    l! dF-
-- /FA_case/       l+ 0
-- /FA_activate/   l+ 0
-- /FA_tinhibitLB/ l- s-
-- /FA_tinhibitHB/ l- 0
-- /FA_inhibit/    l- s-
-- /FA_next/       l+ 0

