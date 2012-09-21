with Support, AndPorP; use Support, AndPorP;

--  covering A and C only but adding more test vectors to verify
--  coverage of B is NOT detected
procedure Test_AndPorP_AC_More is
begin
   -- A: {1, 2}; C: {1, 3}
   Assert (F (True, False, True) = True); -- 1
   Assert (F (False, False, True) = False); -- 2
   Assert (F (True, False, False) = False); -- 3
   Assert (F (False, True, False) = False);
end;

--# andporp.adb
--  /eval(Stmt|Other)/      l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
