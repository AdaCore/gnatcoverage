with Support; use Support;

with Sensors, Sensors.Predicates, Slists, Slists.Count;
use Sensors, Sensors.Predicates, Slists, Slists.Count;

--  Call into Count_In with a non-empty list and a filter
--  such that the If control evaluates both True and False.

procedure Test_Iftf is
   Sin : aliased Sensor (Hist_Size => 5);  -- in range sensor
   Sout : aliased Sensor (Hist_Size => 5); -- out of range sensor
   SL : Sensor_List;
   
   Nt, Nf : Natural;
begin
   
   Sin.V := 5;
   Sin.ALB := 1;
   Sin.AHB := 10;
   
   Prepend (Sin'Unchecked_Access, SL);
   
   Sout.V := 5;
   Sout.ALB := 1;
   Sout.AHB := 3;
   
   Prepend (Sout'Unchecked_Access, SL);
   
   Count_In (Sl, Inrange'Access, Nt, Nf);
   Assert (Nt = 1);
   Assert (Nf = 1);
end;

--# slists-count.adb

-- /CO_decl/   ~l+ ## 0
-- /CO_init/   l+ ## 0
-- /CO_while/  l+ ## 0
-- /CO_test/   l+ ## 0
-- /CO_incT/   l+ ## 0
-- /CO_incF/   l+ ## 0
-- /CO_next/   l+ ## 0

