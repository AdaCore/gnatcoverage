with Ops, Support; use Ops, Support;

procedure Test_Ops_All is
begin
   Assert (not One_Ok (Cs_Ko, Cs_Ko));
   Assert (One_Ok (Cs_Ok, Cs_Ok));
   
   Assert (not One_Ok (Cs_Ov, Cs_Ok));
   Assert (N_Ok = 1);
   Assert (N_Ko = 1);
   Assert (N_Ov = 1);
end;

--# ops.adb
--  /test/  l+ ## 0
--  /ok/    l+ ## 0
--  /ko/    l+ ## 0
--  /ov/    l+ ## 0

