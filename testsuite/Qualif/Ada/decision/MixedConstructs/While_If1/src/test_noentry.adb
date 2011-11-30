with Support; use Support;

with Sensors, Sensors.Predicates, Slists, Slists.Count;
use Sensors, Sensors.Predicates, Slists, Slists.Count;

--  Call into Count_In with an empty list. Loop never entered.

procedure Test_Noentry is
   SL : Sensor_List;
   Nt, Nf : Natural;
begin
   Count_In (Sl, Pass'Access, Nt, Nf);
end;

--# slists-count.adb

-- /CO_decl/   ~l+ 0
-- /CO_init/   l+ 0
-- /CO_while/  l! dT-
-- /CO_test/   l- s-
-- /CO_incT/   l- s-
-- /CO_incF/   l- s-
-- /CO_next/   l- s-

