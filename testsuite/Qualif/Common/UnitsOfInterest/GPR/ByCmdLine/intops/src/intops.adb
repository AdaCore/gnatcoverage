with Counters; use Counters;

package body Intops is

   Eval_Counter : Counter;

   function Add (A, B : Integer) return Integer is separate;
   function Sub (A, B : Integer) return Integer is separate;

   function Eval (Op: Opkind; A, B : Integer) return Integer is
   begin
      Bump (Eval_Counter);
      case Op is
         when Op_Add => return Add (A, B); -- # eval_add
         when Op_Sub => return Sub (A, B); -- # eval_sub
      end case;
   end;

end;
