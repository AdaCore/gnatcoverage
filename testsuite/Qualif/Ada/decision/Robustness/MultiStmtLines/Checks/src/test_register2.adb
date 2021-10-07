with Darts, Register, Support; use Darts, Support;

procedure Test_Register2 is
   G : Game;
begin
   Register (Hit => 20, Double => True, Triple => False, G => G);
   Assert (G.Score = 40);
   Assert (G.Hits = 1);
   Assert (G.Fancy_Hits = 1);
end Test_Register2;

--# register.adb
--  /init/   l! ## s-:"This_Score := 0",dF-
--  /double/ l! ## dF-:"if Double"
--  /triple/ l! ## s-:"This_Score :=",dT-:"if Triple"
--  /hits/   l! ## dF-
--  /times/  l+ ## 0

-- %cargs: -O1
--  =/init/  l! ## s-:"This_Score := 0",dF-

--  Some compilers are imprecise with multiple stmts on a line

-- %tags:(7.1.2|7.2.2)
--  =/init/   l! ## s!, dF-
--  =/triple/ l! ## s!, dT-


