with Support, Ops; use Support, Ops;

--  Call into the internal ops only

procedure Test_Ops_I is
   Opd : Opdata;
begin
   Do_Ops (V => False, P => False, I => True, Opd => Opd);
   Assert (Opd.Nops_Done = 1);
end;

--# ops.adb
--  /touch/ l+ 0
--  /doops/ l+ 0
--  /vsub/  l- s-
--  /psub/  l- s-
--  /isub/  l+ 0

--# ops-vsub.adb
--  /vsub/  l- s-

--# ops-psub.adb
--  /psub/  l- s-

--# ops-isub.adb
--  /isub/  l+ 0

