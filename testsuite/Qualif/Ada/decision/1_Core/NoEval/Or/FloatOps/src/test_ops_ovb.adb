with Ops, Support; use Ops, Support;

procedure Test_Ops_Ovb is
begin
   -- We must put a Ko first to evaluate the overflow
   Assert (not One_Ok (Cs_Ko, Cs_Ov));
   
   Assert (N_Ok = 0);
   Assert (N_Ko = 0);
   Assert (N_Ov = 1);
end;

--# ops.adb
--  /test/  l! ## d-
--  /ok/    l- ## s-
--  /ko/    l- ## s-
--  /ov/    l+ ## 0

