with Support, Ops; use Support, Ops;

procedure Test_Ops_U is
begin
   Assert (Qualify (Step, Pit) = Unsafe);
end;

--# ops.adb
--  /test/    l! ## dF-
--  /unsafe/  l+ ## 0
--  /safe/    l- ## s-
--  /stmt/    l+ ## 0
