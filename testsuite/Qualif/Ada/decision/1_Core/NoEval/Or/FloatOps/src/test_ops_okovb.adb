with Ops, Support; use Ops, Support;

procedure Test_Ops_OkOvb is
begin
   Assert (One_Ok (Cs_Ok, Cs_Ko));
   
   Assert (One_Ok (Cs_Ok, Cs_Ov));
   
   -- The overflow is shortcircuited
   
   Assert (N_Ok = 2);
   Assert (N_Ko = 0);
   Assert (N_Ov = 0);
end;

--# ops.adb
--  /test/  l! ## dF-
--  /ok/    l+ ## 0
--  /ko/    l- ## s-
--  /ov/    l- ## s-

