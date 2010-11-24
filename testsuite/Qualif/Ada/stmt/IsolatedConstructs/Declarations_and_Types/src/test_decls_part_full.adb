--  Test driver for declarations. The goal is to have all the constructs of
--  interest  covered.
--
--  The driver calls the functional subprogram Decls and both Local_1 and
--  Local_2 subprograms from functional package Decls_Pack. All the
--  constructs of interest  should be reported as covered.


with Decls;
with Decls_Pack; use Decls_Pack;
with Support; use Support;

procedure Test_Decls_Part_Full is
   Result : Boolean := False;
begin
   Decls (Result);
   Assert (Result);

   Decls_Pack.Local_1 (Result);
   Assert (not Result);

   Result := Decls_Pack.Local_2 (Result);
   Assert (Result);
end Test_Decls_Part_Full;

--# decls.adb
--  /stmt/ l+ 0

--# decls_pack.adb
--  /code1/ l+ 0
--  /decl1/ ~l+ 0
--  /code2/ l+ 0
--  /decl2/ ~l+ 0

--# decls_pack.ads
--  /dcls/ ~l+ 0
