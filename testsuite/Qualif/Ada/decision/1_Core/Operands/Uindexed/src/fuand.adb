package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Ops_Var : Operands := Ops; -- # decl
   begin
      if Ops_Var (Op_A) > 0 and then Ops_Var (Op_B) > 0 then -- # eval0
         return True; -- # true
      else
         return False;  -- # false
      end if;
   end;
end;
