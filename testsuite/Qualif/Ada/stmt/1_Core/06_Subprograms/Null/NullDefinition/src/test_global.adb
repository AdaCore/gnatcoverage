with Objects; use Objects;

procedure Test_Global is
   O : T_Object;
begin
   Process (O, Local => False);
end;

--# objects.ads objects.adb
--  /test/ l+ ## 0
--  /local/ l- ## s-
--  /global/ l+ ## 0
