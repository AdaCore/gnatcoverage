with Support, Gstacks; use Support;

procedure Test_Istack_Nval is
   package Istacks is new Gstacks (Value => Integer);
   use Istacks;

   S : Istacks.Stack (Size => 1);
   pragma Warnings (Off, S);
begin
   Assert (N_Values (S) = 0);
end;

--# gstacks.adb
--  /Xpush/  l- s-
--  /Xpop/   l- s-
--  /Xnval/  l+ 0
