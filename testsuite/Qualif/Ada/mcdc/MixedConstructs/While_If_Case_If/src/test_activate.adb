with Support; use Support;
with Sensors, Slists.Forall; use Sensors, Slists, Slists.Forall;

procedure Test_Activate is
   S1, S2 : aliased Sensor (Hist_Size => 5);
   SL  : Sensor_List;
begin
   S1.ALB := 1;
   S1.AHB := 10;

   S2.ALB := 5;
   S2.AHB := 15;

   Prepend (S1'Unchecked_Access, SL);
   Prepend (S2'Unchecked_Access, SL);

   --  No sensor in SL active. (Re)activate those Active already. Don't
   --  touch any others.

   Forall_In (SL, Activate, Active_Only => True);
   Assert ((not S1.Active) and then (not S2.Active));

   --  Now activate all

   Forall_In (SL, Activate);
   Assert (S1.Active and then S2.Active);

end;

-- Active  Active_Only  OuterIf   (Action)
-- False   True         False
-- False   False        True      (Activate)

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

