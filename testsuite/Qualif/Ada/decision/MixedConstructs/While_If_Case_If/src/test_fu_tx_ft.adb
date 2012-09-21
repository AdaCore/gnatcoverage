with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_FU_TX_FT is
   SL : Sensor_List;
   S1 : aliased Sensor;
begin
   -- While decision covered from loop over non empty list
   -- First If decision True only from T or else X.
   -- Inner If decision True only from T or else X.
   
   S1.ALB := 1;
   S1.AHB := 15;
   S1.V := 30;    -- > high bound
   Prepend (S1'Unchecked_Access, SL);
   
   for X in False .. True loop
      S1.Active := True;
      Forall_In (SL, Inhibit, Active_Only => X);   
   end loop;
end;

--# slists-forall.adb

-- /FA_init/       l+ ## 0
-- /FA_while/      l+ ## 0
-- /FA_tactive/    l! ## dF-
-- /FA_case/       l+ ## 0
-- /FA_activate/   l- ## s-
-- /FA_tinhibitLB/ l! ## dF-
-- /FA_tinhibitHB/ l! ## 0
-- /FA_inhibit/    l+ ## 0
-- /FA_next/       l+ ## 0

