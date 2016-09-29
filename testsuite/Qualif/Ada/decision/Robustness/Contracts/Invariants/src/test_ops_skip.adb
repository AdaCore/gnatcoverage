with Support, Ops;  use Support, Ops;

procedure Test_Ops_Skip is
   O : T_Int (UB => 5);
begin
   Set (O, V => 3, Count => False);
end;

--# ops.ads

--# ops.adb
--  /stmt/ l+ ## 0
--  /test_count/ l! ## dT-
--  /count/ l- ## s-
--  /check/ l- ## s-
