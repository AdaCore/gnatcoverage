with Or_Eq_And_Not, Support; use Support;

procedure Test_AC_T is
begin
   Assert -- or-1, and-2
     (Or_Eq_And_Not (False, False, False, False) = True);
   
   Assert -- or-3, and-1
     (Or_Eq_And_Not (True, False, True, False) = True);
end;

--# oreqandnot.c
--   /eval/ l! ## c!:"B", c!:"!D"
