with Misc, Support; use Misc, Support;

procedure Test_Aligned_False is
begin
   Assert (not Aligned (1, 0, 0));
end;

--# aligned.c
--  /decision/    l! ## dT-
--  /stmt-true/   l- ## s-
--  /stmt-false/  l+ ## 0
