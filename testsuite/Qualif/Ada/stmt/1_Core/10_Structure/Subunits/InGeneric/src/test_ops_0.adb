with Do_Ops;
with Ops; use Ops;
with Support; use Support;

procedure Test_Ops_0 is
   Opd : Opdata;
begin
   Do_Ops (False, Opd);
   Assert (Opd.Nops_Done = 0);
end;

--# ops.adb
-- /touch/  l- ## s-
--
--# do_ops.adb
-- /doops/  l+ ## 0
-- /gsub/   l- ## s-
--
--# pack.adb
--# pack-separate_subp.adb
-- /gsub/   l- ## s-
