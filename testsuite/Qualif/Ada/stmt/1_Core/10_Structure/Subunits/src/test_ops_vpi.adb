with Support, Ops; use Support, Ops;

--  Call into the visible, private and internal ops

procedure Test_Ops_VPI is
   Opd : Opdata;
begin
   Do_Ops (V => True, P => True, I => True, Opd => Opd);
   Assert (Opd.Nops_Done = 3);
end;

--# ops.adb
--  /touch/ l+ 0
--  /doops/ l+ 0
--  /vsub/  l+ 0
--  /psub/  l+ 0
--  /isub/  l+ 0

--# ops-vsub.adb
--  /vsub/  l+ 0

--# ops-psub.adb
--  /psub/  l+ 0

--# ops-isub.adb
--  /isub/  l+ 0

