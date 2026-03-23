with Support, Validate; use Support, Validate;

procedure Test_Valid_Len is
begin
   Assert (Valid (Valid_Str) = True);
   Assert (Valid (Toolong_Str) = False);
end;

--# validate.adb
-- /retLen/    l+ ## 0
-- /checkLen/  l! ## 0
-- /checkKey/  l! ## c!:"Sptr"
