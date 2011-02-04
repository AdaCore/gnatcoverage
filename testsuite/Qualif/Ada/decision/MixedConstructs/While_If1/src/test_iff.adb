with Support; use Support;

with Sensors, Sensors.Predicates, Slists, Slists.Count;
use Sensors, Sensors.Predicates, Slists, Slists.Count;

--  Call into Count_In with a non-empty list and an always rejecting
--  filter. If control always False.

procedure Test_Iff is
   S : aliased Sensor (Hist_Size => 5);
   SL : Sensor_List;
   Nt, Nf : Natural;
begin
   Prepend (S'Unchecked_Access, SL);
   Count_In (Sl, Nopass'Access, Nt, Nf);
   Assert (Nt = 0);
   Assert (Nf = 1);
end;

--# slists-count.adb

-- /CO_decl/   ~l+ 0
-- /CO_init/   l+ 0
-- /CO_while/  l+ 0
-- /CO_test/   l! d!
-- /CO_incT/   l- s-
-- /CO_incF/   l+ 0
-- /CO_next/   l+ 0

