with Support, Validate; use Support, Validate;

procedure Test_Valid_Key is
begin
   Assert (Valid (Valid_Str) = True);
   Assert (Valid (Invalid_Str) = False);
end;

--# validate.adb
-- /retLen/    l+ 0
-- /checkLen/  l! m!:"Length"
-- /checkKey/  l! 0
