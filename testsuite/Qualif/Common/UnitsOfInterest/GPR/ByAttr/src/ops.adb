with Ops.Andthen; with Ops.Orelse;

package body Ops is

   function Eval (Op: Opkind; A, B : Boolean) return Boolean is
   begin
      case Op is
         when Op_Andthen => return Ops.Andthen.Eval (A, B); -- # eval_and
         when Op_Orelse => return Ops.Orelse.Eval (A, B);   -- # eval_or
      end case;
   end;

end;
