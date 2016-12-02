with Ops, Support; use Ops, Support;

procedure Test_Ops_OkOvb is
begin
   Assert (Both_Ok (Cs_Ok, Cs_Ok));
   
   Assert (not Both_Ok (Cs_Ok, Cs_Ov));
   Assert (N_Ok = 1);
   Assert (N_Ko = 0);
   Assert (N_Ov = 1);
end;

--# ops.adb
--  /test/  l! ## dF-
--  /ok/    l+ ## 0
--  /ko/    l- ## s-
--  /ov/    l+ ## 0

