with Support, Ops; use Support, Ops;

procedure Test_Ok is
   X : Integer;
   Fault : Boolean;
begin
   To_Integer ("54", X, Fault);
   Assert (X = 54 and then not Fault);
end;

--# ops.adb
--  /convert/ l+ ## 0
--  /no_fault/ l+ ## 0
--  /fault/  l- ## s-

