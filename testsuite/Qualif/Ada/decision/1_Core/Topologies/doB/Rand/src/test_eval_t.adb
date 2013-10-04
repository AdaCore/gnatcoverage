with Support, Eval; use Support, Eval;

procedure Test_Eval_T is
begin
   Assert (E_And (True, True) = True);
end;

--# eval.adb
--  /eval/  l! ## oF-
--  /retTrue/  l+ ## 0
--  /retFalse/ l- ## s-
--  /retVal/   l+ ## 0
