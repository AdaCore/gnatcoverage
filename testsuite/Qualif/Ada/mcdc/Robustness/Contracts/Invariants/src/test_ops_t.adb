with Support, Ops;  use Support, Ops;

procedure Test_Ops_T is
   D : T_Double;
begin
   Set (D, X => 3, Y => 3);
   Assert (Bad_Set (D));
end;

--# ops.ads
--  /fn/ l- ## s-

--# ops.adb
--  /stmt/ l+ ## 0
--  /check/ l- ## s-
--  /eval/ l! ## eF-
