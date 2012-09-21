with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_T is
begin
   Assert (Or_Else (True, True) = True);
   Assert (Or_Else (False, True) = True);
   Assert (Or_Else (True, False) = True);
end;

--# orelse.adb
--  /eval(Stmt|Other)/   l! ## oF-
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l- ## s-
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0
