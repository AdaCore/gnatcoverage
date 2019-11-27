with Darts, Register, Support; use Darts, Support;

procedure Test_Register1 is
   G : Game;
begin
   Register
     (Hit => 20, Double => False, Triple => False, G => G);
   Assert (G.Score = 20);
   Assert (G.Hits = 1);
   Assert (G.Fancy_Hits = 0);
end;

--# register.adb
--  /init/   l! ## s-:"This_Score .= 0",dF-:"if Hit > 0"
--  /double/ l! ## s-:"This_Score .=",dT-:"if Double"
--  /triple/ l! ## s-:"This_Score .=",dT-:"if Triple"
--  /hits/   l! ## s-:"G.Fancy_Hits .=",dT-
--  /decl/  ~l- ## ~s-
--  /times/  l- ## s-
  
-- %cargs: -O1
--  =/init/  l! ## s-:"This_Score .= 0",dF-

-- Some compilers are imprecise with multiple stmts on a line

-- %tags:(7.1.2|7.2.2) 
-- =/init/   l! ## s!,d!:"if Hit > 0"
-- =/double/ l! ## s!,d!:"if Double"
-- =/triple/ l! ## s!,d!:"if Triple"
-- =/hits/   l! ## s!,dT-

