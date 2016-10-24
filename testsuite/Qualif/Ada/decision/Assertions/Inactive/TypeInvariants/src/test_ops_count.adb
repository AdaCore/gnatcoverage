with Support, Ops;  use Support, Ops;

procedure Test_Ops_Count is
   O : T_Int (UB => 5);
begin
   Set (O, V => 3, Count => True);
end;

--# ops.ads

--# ops.adb
--  /stmt/ l+ ## 0
--  /test_count/ l! ## dF-
--  /count/ l+ ## 0
--  /check/ l- ## s-
