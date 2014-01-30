with Darts, Support; use Darts, Support;

procedure Test_Darts_1 is
   G : Game;
begin
   Reset (G);
   Register (Hit => 20, Double => False, Triple => False, G => G);
   Assert (G.Score = 20);
   Assert (G.Hits = 1);
   Assert (G.Fancy_Hits = 0);
end;

--# darts.adb
--  /init/   l! ## s!,~s-:"This_Score .= 0",dF-
--  /double/ l! ## s!,~s-:"This_Score .=",dT-
--  /triple/ l! ## s!,~s-:"This_Score .=",dT-
--  /hits/   l! ## s!,~s-:"Fancy_Hits .=",dT-
