with Objects; use Objects;

procedure Test_Local is
   O : T_Object;
begin
   Process (O, Local => True);
end;

--# objects.ads objects.adb
--  /test/ l+ ## 0
--  /local/ l+ ## 0
--  /global/ l- ## s-
