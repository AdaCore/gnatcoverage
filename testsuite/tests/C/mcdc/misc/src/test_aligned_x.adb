with Misc, Support; use Misc, Support;

procedure Test_Aligned_X is
begin
   Assert (Aligned (16, 24, 8));
   Assert (not Aligned (7, 24, 8));
end;

--# aligned.c
--  /eval/ l! ## c!:"!(y"

