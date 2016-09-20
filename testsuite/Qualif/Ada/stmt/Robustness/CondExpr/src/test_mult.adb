with Support, Ops; use Support, Ops;

procedure Test_Mult is
   Z : Integer;
begin
   Add_Or_Mult (5, 3, Z);
   Assert (Z = 15);
end;

--# ops.adb
--  /test/ l+ ## 0
--  /add/  l- ## s-
--  /mult/ l+ ## 0
