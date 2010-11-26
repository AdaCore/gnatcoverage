with Support, AndPorPand_Coupled;
use Support, AndPorPand_Coupled;

--  Covering all conditions but one
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_2 is
begin
   --  Not covering B
   --  F1 : A == D  in
   --  A and then (B or else C) and then D
   --  A : {1, 2}
   --  C : {1, 3}
   Assert (F1 (True, False, True) = True); -- 1
   Assert (F1 (False, True, False) = False); -- 2
   Assert (F1 (True, False, False) = False); -- 3

   --  Not covering B
   --  F2 : B == D  in
   --  A and then (B or else D) and then C
   --  A : {1, 2}
   --  C : {1, 3}
   Assert (F2 (True, True, True) = True); -- 1
   Assert (F2 (False, True, True) = False); -- 2
   Assert (F2 (True, True, False) = False); -- 3

   --  Not covering B
   --  F3 : A == D in
   --  A and then (D or else B) and then C
   --  A : {2, 3}
   --  C : {1, 2} with masking
   Assert (F3 (True, False, False) = False); -- 1
   Assert (F3 (True, True, True) = True); -- 2
   Assert (F3 (False, True, True) = False); -- 3


end Test_AndPorPand_Coupled_2;

--# andporpand_coupled.adb
-- /F1_evaluate/        l! m!:"B"
-- /F1_evaluate/        l! m!:"D"
-- /F1_decisionTrue/    l+ 0
-- /F1_decisionFalse/   l+ 0
-- /F2_evaluate/        l! m!:"B"
-- /F2_evaluate/        l! m!:"D"
-- /F2_decisionTrue/    l+ 0
-- /F2_decisionFalse/   l+ 0
-- /F3_evaluate/        l! m!:"B"
-- /F3_evaluate/        l! m!:"D"
-- /F3_decisionTrue/    l+ 0
-- /F3_decisionFalse/   l+ 0
-- /decl/               ~l+ 0
-- /returnValue/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0

