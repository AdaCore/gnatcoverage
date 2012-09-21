with MultiBackprop, Support; use MultiBackprop, Support;

-- Verify that nothing is reported uncovered when the noop subprogram
-- is called.

procedure Test_MultiBackprop_F is
begin
   P (False);
   Assert (True);
end;

--# multibackprop.adb

--  /alwaysEval/ l+ ## 0
--  /trueEval/   l- ## s-
