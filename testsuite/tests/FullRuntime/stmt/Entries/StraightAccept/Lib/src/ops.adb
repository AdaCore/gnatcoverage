
package body Ops is

   task type Opmaster is
      entry Compute (Op : Opkind; A, B : Boolean; Result : out Boolean);
   end Opmaster;

   task body Opmaster is
   begin
      accept Compute
        (Op : Opkind; A, B : Boolean; Result : out Boolean)
      do
         case Op is
            when Op_And => Result := A and then B; -- # do_and
            when Op_Or => Result := A or else B;   -- # do_or
         end case;
      end;
   end;

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is

      T : Opmaster;
      Result : Boolean;
   begin
      T.Compute (Op, A, B, Result); -- # compute
      return Result;                -- # compute
   end;

end;
