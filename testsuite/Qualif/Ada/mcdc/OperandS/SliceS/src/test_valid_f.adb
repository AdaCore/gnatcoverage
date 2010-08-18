with Support, Validate; use Support, Validate;

procedure Test_Valid_F is
begin
   Assert (Valid (Toolong_Str) = False);
   Assert (Valid (Invalid_Str) = False);
end;

--# validate.adb
-- /retLen/   l+ 0
-- /checkLen/ l! dT-
-- /checkKey/ l! 0
