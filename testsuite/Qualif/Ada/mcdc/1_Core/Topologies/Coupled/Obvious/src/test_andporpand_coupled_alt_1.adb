with Support, AndPorPand_Coupled_Alt;
use Support, AndPorPand_Coupled_Alt;

--  Covering all conditions but one
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_Alt_1 is
begin
   --  Not covering A
   --  F1 : A is coupled  in
   --  A and then (B or else C) and then A'
   --  B : {1, 2}
   --  C : {1, 3}
   Assert (F1 (True, False, False) = False); -- 1
   Assert (F1 (True, True, False) = True); -- 2
   Assert (F1 (True, False, True) = True); -- 3

   --  Not covering A
   --  F2 : B is coupled  in
   --  A and then (B or else B') and then C
   --  B : {1, 2}
   --  C : {2, 3}
   Assert (F2 (True, False, True) = False); -- 1
   Assert (F2 (True, True, True) = True); -- 2
   Assert (F2 (True, True, False) = False); -- 3

   --  Not covering B
   --  F3 : A is coupled in
   --  A and then (A' or else B) and then C
   --  A : {2, 3}
   --  C : {1, 2}
   Assert (F3 (True, False, False) = False); -- 1
   Assert (F3 (True, False, True) = True); -- 2
   Assert (F3 (False, False, True) = False); -- 3


end Test_AndPorPand_Coupled_Alt_1;

--# andporpand_coupled_alt.adb
-- /F1_evaluate/        l! ## c!:"A"
-- /coupF1_evaluate/    l! ## c!:"A"
-- /F1_decisionTrue/    l+ ## 0
-- /F1_decisionFalse/   l+ ## 0
-- /F2_evaluate/        l! ## c!:"A"
-- /coupF2_evaluate/    l! ## c!:"B"
-- /F2_decisionTrue/    l+ ## 0
-- /F2_decisionFalse/   l+ ## 0
-- /F3_evaluate/        l! ## 0
-- /coupF3_evaluate/    l! ## c!:"B"
-- /coupF3_evaluate/    l! ## c!:"A"
-- /F3_decisionTrue/    l+ ## 0
-- /F3_decisionFalse/   l+ ## 0
-- /decl/               ~l+ ## 0
-- /returnValue/        l+ ## 0
-- /returnTrue/         l+ ## 0
-- /returnFalse/        l+ ## 0
