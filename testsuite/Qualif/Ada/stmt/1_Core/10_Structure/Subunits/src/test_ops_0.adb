with Support, Ops; use Support, Ops;

procedure Test_Ops_0 is
   Opd : Opdata;
begin
   Assert (Opd.Nops_Done = 0);
end;

--# ops.adb
--  /touch/ l- ## s-
--  /doops/ l- ## s-
--  /vsub/  l- ## s-
--  /psub/  l- ## s-
--  /isub/  l- ## s-

--# ops-vsub.adb
--  /vsub/  l- ## s-

--# ops-psub.adb
--  /psub/  l- ## s-

--# ops-isub.adb
--  /isub/  l- ## s-

