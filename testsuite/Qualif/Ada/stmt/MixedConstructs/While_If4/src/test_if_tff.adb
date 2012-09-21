with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Test_If_TFF is
   SL  : Sensor_List;
   S : aliased Sensor (Hist_Size =>5);
begin
   Prepend (S'Unchecked_Access, SL);
   
   --  Arrange to 'skip' only
   
   S.Active := False;
   
   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, Active_Only => True,
	       Skipped => Skip, Fault => Fault, Ok => Ok);
      Assert (Skip.Len = 1 and then Fault.Len = 0 and then Ok.Len = 0);
   end;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l+ ## 0
-- /AF_skip/  l+ ## 0
-- /AF_evLB/  l- ## s-
-- /AF_evHB/  l- ## 0c
-- /AF_fault/ l- ## s-
-- /AF_ok/    l- ## s-
-- /AF_next/  l+ ## 0

