with Counters; use Counters;

with Boolops.Andthen; with Boolops.Orelse;

package body Boolops is

   Eval_Counter : Counter;

   function Eval (Op: Opkind; A, B : Boolean) return Boolean is
   begin
      Bump (Eval_Counter);
      case Op is
         when Op_Andthen => return Boolops.Andthen.Eval (A, B); -- # eval_and
         when Op_Orelse => return Boolops.Orelse.Eval (A, B);   -- # eval_or
      end case;
   end;

end;
