with Support;

procedure Test_Afail is
begin
   Support.Assert (False); -- # assert
end;

--# test_afail.adb
--  /assert/ l+ 0
