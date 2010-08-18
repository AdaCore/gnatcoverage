with Support, Odecls; use Support, Odecls;

-- "with" the package with object declarations, don't call the provided
-- subprogram and verify that all and only the subprogram pieces are reported
-- uncovered.

procedure Test_Odecls_0 is
begin
   Assert (True);
end;

--# odecls.ads
--  /stmt/ l+ 0

--# odecls.adb
--  /stmt/ l- s-
