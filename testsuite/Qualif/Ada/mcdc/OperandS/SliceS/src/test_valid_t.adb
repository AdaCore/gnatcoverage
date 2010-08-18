with Support, Validate; use Support, Validate;

procedure Test_Valid_T is
begin
   Assert (Valid (Valid_Str) = True);
end;

--# validate.adb
-- /retLen/   l+ 0
-- /checkLen/ l! dF-
-- /checkKey/ l! 0
