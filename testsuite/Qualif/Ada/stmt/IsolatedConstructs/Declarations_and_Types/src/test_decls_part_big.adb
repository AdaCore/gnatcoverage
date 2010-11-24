--  Test driver for declarations. The goal is to have not all but a "big"
--  part of the constructs of interest  covered.
--
--  The driver calls the functional subprogram Decls and function Local_2 from
--  functional package Decls_Pack. So the only declarations that should be
--  reported as non-covered are local declarations in procedure
--  Decls_Pack.Local_1


with Decls;
with Decls_Pack; use Decls_Pack;
with Support; use Support;

procedure Test_Decls_Part_Big is
   Result : Boolean := False;
begin
   Decls (Result);
   Assert (Result);

   Result := Decls_Pack.Local_2 (Result);
   Assert (not Result);
end Test_Decls_Part_Big;

--# decls.adb
--  /stmt/ l+ 0

--# decls_pack.adb
--  /code1/ l- s-
--  /decl1/ ~l- ~s-
--  /code2/ l+ 0
--  /decl2/ ~l+ 0

--# decls_pack.ads
--  /dcls/ ~l+ 0
