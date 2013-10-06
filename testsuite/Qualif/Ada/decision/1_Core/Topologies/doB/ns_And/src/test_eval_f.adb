with Support, Eval; use Support, Eval;

procedure Test_Eval_F is
begin
   Assert (E_And (False, True) = False);
   Assert (E_And (True, False) = False);
end;

--# eval.adb
--  /eval/  l! ## oT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

