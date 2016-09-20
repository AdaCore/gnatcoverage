with Support, Ops; use Support, Ops;

procedure Test_Add is
   Z : Integer;
begin
   Add_Or_Mult (1, 3, Z);
   Assert (Z = 4);
end;

--# ops.adb
--  /test/ l! ## dF-
--  /add/  l+ ## 0
--  /mult/ l- ## s-
