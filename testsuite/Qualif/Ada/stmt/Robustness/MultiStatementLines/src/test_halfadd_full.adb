with Support, Halfadd; use Support;

procedure Test_Halfadd_Full is
   S, C : Boolean;
begin
   Halfadd (False, False, S, C);
   Assert (S = False and then C = False);
   Halfadd (True, False, S, C);
   Assert (S = True and then C = False);
   Halfadd (False, True, S, C);
   Assert (S = True and then C = False);
   Halfadd (True, True, S, C);
   Assert (S = False and then C = True);
end;

--# halfadd.adb
--  /sum/   l+ ## 0
--  /carry/ l+ ## 0

--  Old versions of the compiler are imprecise on mutli-line SCOs or
--  multi-SCO lines, in particular with optimization. The degraded
--  expectations below are not ideal, but are not a potential safety
--  issue and are not so incorrect for a statement spanning multiple
--  lines.

-- %tags: (7.1.2|7.2.2) %cargs: -O1
-- =/carry/ l! ## s!
