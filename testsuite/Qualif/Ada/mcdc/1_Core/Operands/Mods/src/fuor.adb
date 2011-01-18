package body FUOR is

   function Mod0_Or (OpA, OpB : Modop) return Boolean is
      Ya : Integer renames OpA.Y;
      Yb : Integer renames OpB.Y;
   begin
      return OpA.X mod Ya = 0 -- # evalA
        or else  OpB.X mod Yb = 0; -- # evalB
   end;
end;
