with Support, Odecls; use Support, Odecls;

-- "with" the package with object declarations, call the provided
-- subprogram and verify that nothing is reported uncovered.

procedure Test_Odecls_Full is
begin
   Check_Locals;
   Assert (Global_Msg.Valid);
   Assert (Global_Msg.Len = Global_Msg.Data'Length);
end;

--# odecls.ads
-- /stmt/ l+ 0

--# odecls.adb
-- /stmt/ l+ 0
