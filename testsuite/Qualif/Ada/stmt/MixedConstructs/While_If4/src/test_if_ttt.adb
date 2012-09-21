with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Test_If_TTT is
   SL  : Sensor_List;
   S1, S2, S3 : aliased Sensor (Hist_Size =>5);
begin
   Prepend (S1'Unchecked_Access, SL);   
   Prepend (S2'Unchecked_Access, SL);
   Prepend (S3'Unchecked_Access, SL);
   
   --  Arrange to 'skip', 'fault' and 'ok'
   
   S1.ALB := 1;
   S1.AHB := 1;
   S1.V   := 3;
   S1.Active := False;
   
   S2.ALB := 1;
   S2.AHB := 1;
   S2.V   := 2;
   S2.Active := True;
   
   S3.ALB := 1;
   S3.AHB := 1;
   S3.V   := 1;
   S3.Active := True;
   
   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, Active_Only => True,
	       Skipped => Skip, Fault => Fault, Ok => Ok);
      Assert (Skip.Len = 1 and then Fault.Len = 1 and then Ok.Len = 1);
   end;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l+ ## 0
-- /AF_skip/  l+ ## 0
-- /AF_evLB/  l+ ## 0
-- /AF_evHB/  l+ ## 0
-- /AF_fault/ l+ ## 0
-- /AF_ok/    l+ ## 0
-- /AF_next/  l+ ## 0

