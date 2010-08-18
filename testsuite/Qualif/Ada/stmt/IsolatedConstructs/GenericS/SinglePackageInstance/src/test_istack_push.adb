with Support, Istacks; use Support, Istacks;

procedure Test_Istack_Push is
   S : Istacks.Stack (Size => 1);
begin
   Push (5, S);
end;

--# gstacks.adb
--  /Xpush/  l+ 0
--  /Xpop/   l- s-
--  /Xnval/  l- s-
