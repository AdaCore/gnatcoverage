with Support, Value; use Support, Value;

procedure Test_Value_T is
begin
   Assert (F (True) = True);
end;

--# value.adb
-- /eval/    l! ## oF-
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l- ## s-
-- /returnVal/   l+ ## 0

--  With binary traces, we can't tell the difference between dT- or dF- on
--  unary expressions in if-expressions. With source instrumentation, we can.

-- %opts: --trace-mode=src
-- /ifx-eval/    l! ## dF-

-- %opts: --trace-mode=bin
-- /ifx-eval/    l! ## d!
