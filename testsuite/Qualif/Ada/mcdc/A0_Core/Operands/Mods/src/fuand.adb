package body FUAND is

   function Mod0_And (OpA, OpB : Modop) return Boolean is
      Ya : Integer renames OpA.Y; -- # decl
      Yb : Integer renames OpB.Y; -- # decl
   begin
      return OpA.X mod Ya = 0 -- # evalA
        and then OpB.X mod Yb = 0; -- # evalB
   end;
end;
