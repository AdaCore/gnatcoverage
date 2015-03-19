with Do_Ops;
with Ops; use Ops;
with Support; use Support;

procedure Test_Ops_0 is
   Opd : Opdata;
begin
   Do_Ops (True, Opd);
   Assert (Opd.Nops_Done = 1);
end;

--# ops.adb
-- /touch/  l+ ## 0
--
--# do_ops.adb
-- /doops/  l+ ## 0
-- /gsub/   l+ ## 0
--
--# pack.adb
--# pack-separate_subp.adb
-- /gsub/   l+ ## 0
