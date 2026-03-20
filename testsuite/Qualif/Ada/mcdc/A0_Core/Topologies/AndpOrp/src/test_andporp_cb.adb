with Support, AndPorP; use Support, AndPorP;

--  covering B and C only
procedure Test_AndPorP_CB is
begin
   -- B: {1, 2}; C: {2, 3}
   Assert (F (True, True, False) = True); -- 1
   Assert (F (True, False, False) = False); -- 2
   Assert (F (True, False, True) = True); -- 3
end;

--# andporp.adb andporp.ads
--  /eval(Stmt|Other)/      l! ## c!:"A"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
