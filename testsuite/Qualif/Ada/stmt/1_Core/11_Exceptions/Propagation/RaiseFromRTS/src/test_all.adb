with Support, Ops; use Support, Ops;

procedure Test_All is
   X : Integer;
   Fault : Boolean;
begin
   To_Integer ("36", X, Fault);
   Assert (X = 36 and then not Fault);
   To_Integer ("blue", X, Fault);
   Assert (X = 0 and then Fault);
end;

--# ops.adb
--  /convert/ l+ ## 0
--  /no_fault/ l+ ## 0
--  /fault/  l+ ## 0

