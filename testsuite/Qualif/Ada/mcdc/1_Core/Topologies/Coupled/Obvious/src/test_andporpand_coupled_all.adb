with Support, AndPorPand_Coupled;
use Support, AndPorPand_Coupled;

--  Try to cover all conditions
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_All is
begin
   --  Not covering D
   --  F1 : A == D  in
   --  A and then (B or else C) and then D
   Assert (F1 (True, True, True) = True);
   Assert (F1 (True, True, False) = True);
   Assert (F1 (True, False, True) = True);
   Assert (F1 (True, False, False) = False);
   Assert (F1 (False, True, True) = False);
   Assert (F1 (False, True, False) = False);
   Assert (F1 (False, False, True) = False);
   Assert (F1 (False, False, False) = False);

   --  Not covering D
   --  F2 : B == D  in
   --  A and then (B or else D) and then C
   Assert (F2 (True, True, True) = True);
   Assert (F2 (True, True, False) = False);
   Assert (F2 (True, False, True) = False);
   Assert (F2 (True, False, False) = False);
   Assert (F2 (False, True, True) = False);
   Assert (F2 (False, True, False) = False);
   Assert (F2 (False, False, True) = False);
   Assert (F2 (False, False, False) = False);

   --  Not covering B and D
   --  F3 : A == D in
   --  A and then (D or else B) and then C
   Assert (F3 (True, True, True) = True);
   Assert (F3 (True, True, False) = False);
   Assert (F3 (True, False, True) = True);
   Assert (F3 (True, False, False) = False);
   Assert (F3 (False, True, True) = False);
   Assert (F3 (False, True, False) = False);
   Assert (F3 (False, False, True) = False);
   Assert (F3 (False, False, False) = False);


end Test_AndPorPand_Coupled_All;

--# andporpand_coupled.adb
-- /F1_evaluate/        l! ## c!:"D"
-- /F1_decisionTrue/    l+ ## 0
-- /F1_decisionFalse/   l+ ## 0
-- /F2_evaluate/        l! ## c!:"D"
-- /F2_decisionTrue/    l+ ## 0
-- /F2_decisionFalse/   l+ ## 0
-- /F3_evaluate/        l! ## c!:"B"
-- /F3_evaluate/        l! ## c!:"D"
-- /F3_decisionTrue/    l+ ## 0
-- /F3_decisionFalse/   l+ ## 0
-- /decl/               ~l+ ## 0
-- /returnValue/        l+ ## 0
-- /returnTrue/         l+ ## 0
-- /returnFalse/        l+ ## 0

