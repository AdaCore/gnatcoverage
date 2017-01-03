pragma Ada_2012;
package body FUAND is
 
   function Andthen (Ops : Operands) return Boolean is
      Aop : Control renames Ops.A; -- # decl
      Bop : Control renames Ops.B; -- # decl
   begin
      return Ops.A.X in Plus1(Aop.Y) | Twice(Aop.Y) -- # evalA
        and then Ops.B.X in Plus1(Bop.Y) | Twice(Bop.Y); -- # evalB
   end;
end;

