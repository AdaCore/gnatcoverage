with Support, Halfadd; use Support;

procedure Test_Halfadd_FT is
   S, C : Boolean;
begin
   Halfadd (False, True, S, C);
   Assert (S = True and then C = False);
end;

--# halfadd.adb
--  /sum/   l+ ## 0
--  /carry/ l! ## s-:"Carry := True"

-- %tags: (7.1.2|7.2.2), bin-traces
-- =/carry/ l! ## s!
