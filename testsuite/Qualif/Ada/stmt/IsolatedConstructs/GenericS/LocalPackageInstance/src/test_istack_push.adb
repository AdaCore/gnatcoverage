with Support, Gstacks; use Support;

procedure Test_Istack_Push is
   package Istacks is new Gstacks (Value => Integer);
   use Istacks;

   S : Istacks.Stack (Size => 1);
begin
   Push (5, S);
end;

--# gstacks.adb
--  /Xpush/  l+ 0
--  /Xpop/   l- s-
--  /Xnval/  l- s-
