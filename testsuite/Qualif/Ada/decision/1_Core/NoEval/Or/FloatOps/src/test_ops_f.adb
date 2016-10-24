with Ops, Support; use Ops, Support;

procedure Test_Ops_F is
begin
   Assert (not One_Ok (Cs_Ko, Cs_Ko));
   Assert (N_Ok = 0);
   Assert (N_Ko = 1);
   Assert (N_Ov = 0);
end;

--# ops.adb
--  /test/  l! ## dT-
--  /ok/    l- ## s-
--  /ko/    l+ ## 0
--  /ov/    l- ## s-

