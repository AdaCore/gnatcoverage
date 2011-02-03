with Support; use Support;

with Sensors, Sensors.Predicates, Slists, Slists.Count;
use Sensors, Sensors.Predicates, Slists, Slists.Count;

--  Call into Count_In with a non-empty list and an always accepting
--  filter. If control always True.

procedure Test_Ift is
   S : aliased Sensor (Hist_Size => 5);
   SL : Sensor_List;
   Nt, Nf : Natural;
begin
   Prepend (S'Unchecked_Access, SL);
   Prepend (S'Unchecked_Access, SL);
   Count_In (Sl, Pass'Access, Nt, Nf);
   Assert (Nt = 2);
   Assert (Nf = 0);
end;

--# slists-count.adb

-- /CO_decl/   ~l+ 0
-- /CO_init/   l+ 0
-- /CO_while/  l+ 0
-- /CO_test/   l+ 0
-- /CO_incT/   l+ 0
-- /CO_incF/   l- s-
-- /CO_next/   l+ 0

