
package body Ops is

   task type Opmaster (Op : Opkind) is
      entry Compute (A, B : Boolean; Result : out Boolean);
   end Opmaster;

   task body Opmaster is
   begin
      loop
         select -- # compute
            accept Compute (A, B : Boolean; Result : out Boolean) do -- # compute
               case Op is -- # compute
                  when Op_And => Result := A and then B; -- # do_and
                  when Op_Or  => Result := A or else B;  -- # do_or
               end case;
            end;
         or
            terminate;
         end select;
      end loop;
   end;



   function Compute (Op : Opkind; A, B : Boolean) return Boolean is
      T_And : Opmaster (Op_And);
      T_Or : Opmaster (Op_Or);
      Result : Boolean;
   begin
      case Op is -- # compute
         when Op_And => T_And.Compute (A, B, Result); -- # do_and
         when Op_Or  => T_Or.Compute (A, B, Result);  -- # do_or
      end case;
      return Result; -- # compute
   end;

end;
