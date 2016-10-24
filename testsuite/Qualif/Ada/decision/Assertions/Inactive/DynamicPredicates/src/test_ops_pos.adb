with Ops, Support; use Ops, Support;

procedure Test_Ops_Pos is
begin
   Check (P => (2, 4));
   Assert (N_Positives = 1);
end;

--# ops.ads
--  /check/ l. ## 0

--# ops.adb
--  /test_pos/ l! ## dF-
--  /pos/ l+ ## 0
