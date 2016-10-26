package body FUOR is
   
   function Orelse (Ops : Operands) return Boolean is
      A : Ref renames Ops.A; -- # decl
      B : Ref renames Ops.B; -- # decl
   begin
      if A.Data(A.Index) = 'T' or else B.Data(B.Index) = 'T' then -- # eval0
         return True; -- # true
      else
         return False;  -- # false
      end if;
   end;
end;
