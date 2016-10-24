with Ops, Support; use Ops, Support;

procedure Test_Ops_T is
begin
   Assert (Both_Ok (Cs_Ok, Cs_Ok));
   Assert (N_Ok = 1);
   Assert (N_Ko = 0);
   Assert (N_Ov = 0);
end;

--# ops.adb
--  /test/  l! ## dF-
--  /ok/    l+ ## 0
--  /ko/    l- ## s-
--  /ov/    l- ## s-

