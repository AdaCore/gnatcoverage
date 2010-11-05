with Support, Halfadd; use Support;

procedure Test_Halfadd_FF is
   S, C : Boolean;
begin
   Halfadd (False, False, S, C);
   Assert (S = False and then C = False);
end;

--# halfadd.adb
--  /sum/   l+ 0
--  /carry/ l! s!
