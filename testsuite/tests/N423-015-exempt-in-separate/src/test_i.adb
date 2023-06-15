with Pck, Assert;

procedure Test_I is
begin
   Pck.Check (Valid => False);
   Assert (Pck.Valids = 0);
   Assert (Pck.Invalids = 1);
end;

--# pck.adb

--# pck-check.adb
--  /valids/   l- ## s-
--  /invalids/ l# ## x0
