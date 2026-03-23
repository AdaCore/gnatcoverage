with Misc, Support; use Misc, Support;

procedure Test_Aligned_True is
begin
   Assert (Aligned (0, 0, 0));
end;

--# aligned.c
--  /decision/    l! ## dF-
--  /stmt-true/   l+ ## 0
--  /stmt-false/  l- ## s-
