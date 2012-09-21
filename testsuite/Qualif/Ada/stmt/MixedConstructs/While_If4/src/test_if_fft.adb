with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Test_If_FFT is
   SL  : Sensor_List;
   S : aliased Sensor (Hist_Size =>5);
begin
   Prepend (S'Unchecked_Access, SL);
   
   --  Arrange to 'ok' only
   
   S.ALB := 1;
   S.AHB := 1;
   S.V   := 1;
   S.Active := True;
   
   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, Active_Only => True,
	       Skipped => Skip, Fault => Fault, Ok => Ok);
      Assert (Skip.Len = 0 and then Fault.Len = 0 and then Ok.Len = 1);
   end;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l+ ## 0
-- /AF_skip/  l- ## s-
-- /AF_evLB/  l+ ## 0
-- /AF_evHB/  l+ ## 0
-- /AF_fault/ l- ## s-
-- /AF_ok/    l+ ## 0
-- /AF_next/  l+ ## 0

