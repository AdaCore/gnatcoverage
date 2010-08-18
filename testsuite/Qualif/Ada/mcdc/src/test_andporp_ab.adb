with Support, AndPorP; use Support, AndPorP;

--  covering A and B only
procedure Test_AndPorP_AB is
begin
   -- A: {1, 2}; B: {1, 3}
   Assert (F (True, True, False) = True); -- 1
   Assert (F (False, True, False) = False); -- 2
   Assert (F (True, False, False) = False); -- 3
end;

--# andporp.adb
--  /evaluate/      l! c!:"C"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
