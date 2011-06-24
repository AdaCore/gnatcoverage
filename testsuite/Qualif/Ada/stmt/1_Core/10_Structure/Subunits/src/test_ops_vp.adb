with Support, Ops; use Support, Ops;

--  Call into the visible and private ops only

procedure Test_Ops_VP is
   Opd : Opdata;
begin
   Do_Ops (V => True, P => True, I => False, Opd => Opd);
   Assert (Opd.Nops_Done = 2);
end;

--# ops.adb
--  /touch/ l+ 0
--  /doops/ l+ 0
--  /vsub/  l+ 0
--  /psub/  l+ 0
--  /isub/  l- s-

--# ops-vsub.adb
--  /vsub/  l+ 0

--# ops-psub.adb
--  /psub/  l+ 0

--# ops-isub.adb
--  /isub/  l- s-

