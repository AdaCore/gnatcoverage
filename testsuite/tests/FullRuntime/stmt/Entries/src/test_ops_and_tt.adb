with Support, Ops; use Support, Ops;

procedure Test_Ops_And_TT is
begin
   Assert (Ops.Compute (Op_And, True, True) = True);
end;


--# ops.adb
-- /do_and/            l+ ## 0
-- /do_or/             l- ## s-
-- /compute/           l+ ## 0
-- /kill/              l- ## s-
-- /sometimes-timeout/ l+ ## 0
-- /never-timeout/     l- ## s-
