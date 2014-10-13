with Darts, Register, Support; use Darts, Support;

procedure Test_Register1 is
   G : Game;
begin
   Register (Hit => 20, Double => True, Triple => False, G => G);
   Assert (G.Score = 40);
   Assert (G.Hits = 1);
   Assert (G.Fancy_Hits = 1);
end;

--  With multiple stmts on a line, there usually remains imprecise debug
--  slocs that prevent accurate dc assessment on single cond decisions. We
--  thus typically expect d! for decisions take only one way.

--# register.adb
--  /init/   l! ## s-:"This_Score .= 0",dF-
--  /double/ l! ## dF-:"if Double"
--  /triple/ l! ## s-:"This_Score .=",dT-:"if Triple"
--  /hits/   l! ## dF-
--  /times/  l+ ## 0
  
-- %cargs: -O1
--  =/init/  l! ## s-:"This_Score .= 0",dF-
