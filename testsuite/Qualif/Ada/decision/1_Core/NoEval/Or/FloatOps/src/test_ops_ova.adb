with Ops, Support; use Ops, Support;

procedure Test_Ops_Ova is
begin
   Assert (not One_Ok (Cs_Ov, Cs_Ok));
   Assert (N_Ok = 0);
   Assert (N_Ko = 0);
   Assert (N_Ov = 1);
end;

--# ops.adb
--  /test/  l! ## d-
--  /ok/    l- ## s-
--  /ko/    l- ## s-
--  /ov/    l+ ## 0
