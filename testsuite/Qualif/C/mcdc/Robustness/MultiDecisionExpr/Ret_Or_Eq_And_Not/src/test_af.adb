with Or_Eq_And_Not, Support; use Support;

procedure Test_AF is
begin
   Assert (Or_Eq_And_Not (False, False, False, False) = True);
   Assert (Or_Eq_And_Not (True, False, False, False) = False);
end;

--# oreqandnot.c
--   /eval/ l! ## c!:"B", eT-:"C"
