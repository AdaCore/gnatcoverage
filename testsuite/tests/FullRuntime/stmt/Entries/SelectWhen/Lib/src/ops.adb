
package body Ops is

   task type Opmaster is
      entry Push_Opkind (O : Opkind);
      entry Compute_And (A, B : Boolean; Result : out Boolean);
      entry Compute_Or (A, B : Boolean; Result : out Boolean);
   end Opmaster;

   task body Opmaster is
      Op : Opkind;
   begin
      accept Push_Opkind (O : Opkind) do -- # compute
         Op := O;  -- # compute
      end Push_Opkind;

      select -- # compute
         when Op = Op_And =>
            accept Compute_And (A, B : Boolean; Result : out Boolean) do -- # do_and
               Result := A and then B; -- # do_and
            end;
      or
         when Op = Op_Or =>
            accept Compute_Or (A, B : Boolean; Result : out Boolean) do -- # do_or
               Result := A or else B;  -- # do_or
            end;
      end select;
   end;

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is

      T : Opmaster;
      Result : Boolean;
   begin
      T.Push_Opkind (Op); -- # compute

      case Op is -- # compute
         when Op_And => T.Compute_And (A, B, Result); -- # do_and
         when Op_Or => T.Compute_Or (A, B, Result); -- # do_or
      end case;
      return Result; -- # compute
   end;

end;
