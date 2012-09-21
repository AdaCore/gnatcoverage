with Support, Halfadd; use Support;

procedure Test_Halfadd_TT is
   S, C : Boolean;
begin
   Halfadd (True, True, S, C);
   Assert (S = False and then C = True);
end;

--# halfadd.adb
--  /sum/   l+ ## 0
--  /carry/ l! ## s!
