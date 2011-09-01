with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Test_Noentry is
   SL  : Sensor_List;
begin
   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, False, Skip, Fault, Ok);
   end;
end;

--# slists-fault.adb

-- /AF_decl/  l+ 0
-- /AF_init/  l+ 0
-- /AF_while/ l+ 0
-- /AF_ren/   l- s-
-- /AF_evA/   l- s-
-- /AF_skip/  l- s-
-- /AF_evLB/  l- s-
-- /AF_evHB/  l- 0c
-- /AF_fault/ l- s-
-- /AF_ok/    l- s-
-- /AF_next/  l- s-

