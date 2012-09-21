with MultiBackprop, Support; use MultiBackprop, Support;

--  When the noop subprogram is not called, its statement body only
--  is expected uncovered.

procedure Test_MultiBackprop_0 is
begin
   Assert (True);
end;

--# multibackprop.adb

--  /alwaysEval/ l- ## s-
--  /trueEval/   l- ## s-
