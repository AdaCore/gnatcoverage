with Support, Istacks; use Support, Istacks;

procedure Test_Istack_Nval is
   S : Istacks.Stack (Size => 1);
   pragma Warnings (Off, S);
begin
   Assert (N_Values (S) = 0);
end;

--# gstacks.adb
--  /Xpush/  l- s-
--  /Xpop/   l- s-
--  /Xnval/  l+ 0
