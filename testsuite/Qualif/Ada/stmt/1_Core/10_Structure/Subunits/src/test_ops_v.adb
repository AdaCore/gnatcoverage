with Support, Ops; use Support, Ops;

--  Call into the visible ops only

procedure Test_Ops_V is
   Opd : Opdata;
begin
   Do_Ops (V => True, P => False, I => False, Opd => Opd);
   Assert (Opd.Nops_Done = 1);
end;

--# ops.adb
--  /touch/ l+ ## 0
--  /doops/ l+ ## 0
--  /vsub/  l+ ## 0
--  /psub/  l- ## s-
--  /isub/  l- ## s-

--# ops-vsub.adb
--  /vsub/  l+ ## 0

--# ops-psub.adb
--  /psub/  l- ## s-

--# ops-isub.adb
--  /isub/  l- ## s-

