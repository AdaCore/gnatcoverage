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
--  /times/  l- ## s-
  
--  7.0.3 is imprecise with multiple stmts on a line

-- %tags:7.0.3 %cargs:gnatp
-- =/init/   l! ## s!,d!:"if Hit > 0"
-- =/double/ l! ## s!,d!:"if Double"
-- =/triple/ l! ## s!,d!:"if Triple"
-- =/hits/   l! ## s!,dT-

-- %tags:7.0.3 %cargs:!gnatp
-- =/init/   l! ## s!,d!:"if Hit > 0"
-- =/double/ l! ## s!,d!:"if Double"
-- =/triple/ l! ## dF-:"if Triple"
-- =/hits/   l! ## s!,dT-

-- Note that the expected  dF- for 7.0.3 !gnatp on "triple"
-- matches an inaccuracy. What we'd like to get is dT-, but
-- the debug info produced by 7.0.3 confuses the tool. We get
-- a violation diagnostic nevertheless, and inaccuracies are
-- documented as may-happen events in our area (multistmt lines).

-- %cargs: -O1
--  =/init/  l! ## s-:"This_Score .= 0",dF-


