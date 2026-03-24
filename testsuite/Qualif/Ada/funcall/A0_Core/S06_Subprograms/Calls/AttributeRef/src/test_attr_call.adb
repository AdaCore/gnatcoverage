with Attr;

procedure Test_Attr_Call
is
    Dummy : String := Attr ("Foo");
begin
    null;
end Test_Attr_Call;

--# attr.adb
-- /id/     l+ ## 0
-- /fun/    l+ ## 0
-- /attr/   l+ ## 0
