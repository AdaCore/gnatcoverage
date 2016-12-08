with Support, Ops; use Support, Ops;

procedure Test_Ops_Fault is
   T : Integer;
   Fault : Boolean;
begin
   Div (1, 0, T, Fault);
   Assert (Fault);
end;

--# ops.adb
--  /fault/ l+ ## 0
--  /no_fault/ l- ## s-
--  /stmt/  l+ ## 0
--  /bad_handler/ l- ## s-
