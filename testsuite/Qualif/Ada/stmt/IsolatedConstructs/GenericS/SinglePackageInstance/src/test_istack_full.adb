with Support, Istacks; use Support, Istacks;

procedure Test_Istack_Full is
   S : Istacks.Stack (Size => 2);
   X : Integer;
begin
   Push (5, S);
   Push (7, S);
   Assert (N_Values (S) = 2);
   Pop (X, S);
   Assert (X = 7);
   Assert (N_Values (S) = 1);
   Pop (X, S);
   Assert (X = 5);
   Assert (N_Values (S) = 0);
end;

--# gstacks.adb
--  /Xpush/  l+ 0
--  /Xpop/   l+ 0
--  /Xnval/  l+ 0
