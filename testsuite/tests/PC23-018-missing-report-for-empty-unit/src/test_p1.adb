with Ops, Ops.P1;

procedure Test_P1 is
   X : Integer := 12;
begin
   Ops.P1.Inc (X);
end;

--# ops.ads
-- /pragma/ l. ## 0

--# ops-p1.adb
-- /stmt/ l+ ## 0
