package body Ops is

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is

      task T is
         entry Compute (Op : Opkind; A, B : Boolean; Result : out Boolean);
      end;

      task body T is
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

      Result : Boolean;
   begin
      T.Compute (Op, A, B, Result); -- # compute
      return Result;                -- # compute
   end;

end;
