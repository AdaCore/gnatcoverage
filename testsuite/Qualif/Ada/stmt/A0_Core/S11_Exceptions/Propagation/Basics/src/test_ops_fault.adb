with Support, Ops; use Support, Ops;

procedure Test_Ops_Fault is
   T : Integer;
   Fault : Boolean;
begin
   Div (1, 0, T, Fault);
   Assert (Fault);

   T := Integer'Last;
   Bump (T, Fault);
   Assert (Fault);
end;

--# ops.adb
--  /div_fault/ l+ ## 0
--  /no_div_fault/ l- ## s-
--  /stmt/  l+ ## 0
--  /bad_handler/ l- ## s-

--  /do_bump/ l+ ## 0
--  /bump_fault/ l+ ## 0
--  /no_bump_fault/ l- ## s-
