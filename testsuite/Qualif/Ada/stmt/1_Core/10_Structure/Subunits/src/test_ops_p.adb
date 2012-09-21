with Support, Ops; use Support, Ops;

--  Call into the private ops only

procedure Test_Ops_P is
   Opd : Opdata;
begin
   Do_Ops (V => False, P => True, I => False, Opd => Opd);
   Assert (Opd.Nops_Done = 1);
end;

--# ops.adb
--  /touch/ l+ ## 0
--  /doops/ l+ ## 0
--  /vsub/  l- ## s-
--  /psub/  l+ ## 0
--  /isub/  l- ## s-

--# ops-vsub.adb
--  /vsub/  l- ## s-

--# ops-psub.adb
--  /psub/  l+ ## 0

--# ops-isub.adb
--  /isub/  l- ## s-

