with Pck, Assert;

procedure Test_V is
begin
   Pck.Check (Valid => True);
   Assert (Pck.Valids = 1);
   Assert (Pck.Invalids = 0);
end;

--# pck.adb

--# pck-check.adb
--  /valids/       l+ ## 0
--  /invalids/     l* ## x+:"Invalids unexpected"
--  /invalids_inc/ l= ## Xs-
