with Support, Ops; use Support, Ops;

procedure Test_fault is
   X : Integer;
   Fault : Boolean;
begin
   To_Integer ("blue", X, Fault);
   Assert (X = 0 and then Fault);
end;

--# ops.adb
--  /convert/ l+ ## 0
--  /no_fault/ l- ## s-
--  /fault/  l+ ## 0

