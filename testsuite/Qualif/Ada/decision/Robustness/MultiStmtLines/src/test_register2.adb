with Darts, Register, Support; use Darts, Support;

procedure Test_Register1 is
   G : Game;
begin
   Register (Hit => 20, Double => True, Triple => False, G => G);
   Assert (G.Score = 40);
   Assert (G.Hits = 1);
   Assert (G.Fancy_Hits = 1);
end;

--# register.adb
--  /init/   l! ## s-:"This_Score .= 0",dF-
--  /double/ l! ## dF-:"if Double"
--  /triple/ l! ## s-:"This_Score .=",dT-:"if Triple"
--  /hits/   l! ## dF-
--  /times/  l+ ## 0

--  7.0.3 is imprecise with multiple stmts on a line

-- %tags:7.0.3
--  =/init/   l! ## s!, d!
--  =/double/ l! ## s!, d!
--  =/triple/ l! ## s!, d!
--  =/hits/   l! ## s!, dF-

-- %cargs: -O1
--  =/init/  l! ## s-:"This_Score .= 0",dF-
