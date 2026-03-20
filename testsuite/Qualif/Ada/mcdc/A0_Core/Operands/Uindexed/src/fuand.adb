pragma Ada_2012;

package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Ops_Var : Operands := Ops; -- # decl
   begin
      return Ops_Var (A) > 0 -- # evalA
        and then Ops_Var (B) > 0; -- # evalB
   end;
end;
