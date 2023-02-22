with Support, Ops; use Support, Ops;

procedure Test_Ops_NoFault is
   T : Integer;
   Fault : Boolean;
begin
   Div (4, 2, T, Fault);
   Assert (T = 2);
   Assert (not Fault);

   Bump (T, Fault);
   Assert (T = 3);
   Assert (not Fault);
end;

--# ops.adb
--  /div_fault/ l- ## s-
--  /no_div_fault/ l+ ## 0
--  /stmt/  l+ ## 0
--  /bad_handler/ l- ## s-

--  /do_bump/ l+ ## 0
--  /bump_fault/ l- ## s-
--  /no_bump_fault/ l+ ## 0
