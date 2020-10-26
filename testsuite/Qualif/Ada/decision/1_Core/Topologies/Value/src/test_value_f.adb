with Support, Value; use Support, Value;

procedure Test_Value_F is
begin
   Assert (F (False) = False);
end;

--# value.adb
-- /eval/    l! ## oT-
-- /returnTrue/  l- ## s-
-- /returnFalse/ l+ ## 0
-- /returnVal/   l+ ## 0

-- %opts: --trace-mode=src
-- /ifx-eval/    l! ## dT-

-- %opts: --trace-mode=bin
-- /ifx-eval/    l! ## d!
