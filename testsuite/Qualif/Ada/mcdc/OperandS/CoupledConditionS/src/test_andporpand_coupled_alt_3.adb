with Support, AndPorPand_Coupled_Alt;
use Support, AndPorPand_Coupled_Alt;

--  Covering all conditions but one
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_Alt_3 is
begin
   --  Not covering C
   --  F1 : A coupled in
   --  A and then (B or else C) and then A'
   --  B : {1, 2}
   --  A : {2, 3}
   Assert (F1 (True, False, False) = False); -- 1
   Assert (F1 (True, True, False) = True); -- 2
   Assert (F1 (False, True, False) = False); -- 3

   --  Not covering C
   --  F2 : B coupled in
   --  A and then (B or else B') and then C
   --  A : {1, 2}
   --  B : {1, 3}
   Assert (F2 (True, True, True) = True); -- 1
   Assert (F2 (False, True, True) = False); -- 2
   Assert (F2 (True, False, True) = False); -- 3

   --  Not covering B and C
   --  F3 : A coupled in
   --  A and then (A' or else B) and then C
   --  A : {1, 2}
   Assert (F3 (True, False, True) = True); -- 1
   Assert (F3 (False, False, True) = False); -- 2
   Assert (F3 (False, True, True) = False); -- 3


end Test_AndPorPand_Coupled_Alt_3;

--# andporpand_coupled_alt.adb
-- /F1_evaluate/        l! m!:"C"
-- /coupF1_evaluate/    l! m!:"A"
-- /F1_decisionTrue/    l+ 0
-- /F1_decisionFalse/   l+ 0
-- /F2_evaluate/        l! 0
-- /coupF2_evaluate/    l! m!:"C"
-- /coupF2_evaluate/    l! m!:"B"
-- /F2_decisionTrue/    l+ 0
-- /F2_decisionFalse/   l+ 0
-- /F3_evaluate/        l! 0
-- /coupF3_evaluate/    l! m!:"A"
-- /coupF3_evaluate/    l! m!:"B"
-- /coupF3_evaluate/    l! m!:"C"
-- /F3_decisionTrue/    l+ 0
-- /F3_decisionFalse/   l+ 0
-- /decl/               ~l+ 0
-- /returnValue/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0

