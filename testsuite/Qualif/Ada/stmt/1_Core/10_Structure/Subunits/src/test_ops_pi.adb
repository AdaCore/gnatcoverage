with Support, Ops; use Support, Ops;

--  Call into the private and internal ops only

procedure Test_Ops_PI is
   Opd : Opdata;
begin
   Do_Ops (V => False, P => True, I => True, Opd => Opd);
   Assert (Opd.Nops_Done = 2);
end;

--# ops.adb
--  /touch/ l+ 0
--  /doops/ l+ 0
--  /vsub/  l- s-
--  /psub/  l+ 0
--  /isub/  l+ 0

--# ops-vsub.adb
--  /vsub/  l- s-

--# ops-psub.adb
--  /psub/  l+ 0

--# ops-isub.adb
--  /isub/  l+ 0

