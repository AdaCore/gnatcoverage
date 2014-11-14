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

-- %tags: (7.0.3|7.2.2)
-- =/carry/ l! ## s!
