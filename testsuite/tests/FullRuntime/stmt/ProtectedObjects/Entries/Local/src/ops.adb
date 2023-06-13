
package body Ops is

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is

      protected type Opmaster is
         entry Take_And;
         entry Take_Or;
         procedure Release;
         procedure Compute (A, B : Boolean; Result : out Boolean);

      private
         Op    : Opkind;           -- # decl
         Free  : Boolean := True;  -- # decl
         Count : Natural := 0;     -- # decl
      end Opmaster;

      protected body Opmaster is
         entry Take_And when Op = Op_And or else Count = 0 is
         begin
            Count := Count + 1;  -- # do_and
            Op := Op_And; -- # do_and
         end Take_And;

         entry Take_Or when Op = Op_Or or else Count = 0 is
         begin
            Count := Count + 1; -- # do_or
            Op := Op_Or; -- # do_or
         end Take_Or;

         procedure Release is
         begin
            Count := Count - 1;  -- # compute
         end Release;

         procedure Compute (A, B : Boolean; Result : out Boolean) is
         begin
            case Op is -- # compute
               when Op_And => Result := A and then B; -- # do_and
               when Op_Or  => Result := A or else B;  -- # do_or
            end case;
         end Compute;
      end Opmaster;

      PO : Opmaster; -- # compute
      Result : Boolean;
   begin
      case Op is -- # compute
         when Op_And => PO.Take_And; -- # do_and
         when Op_Or  => PO.Take_Or;  -- # do_or
      end case;
      PO.Compute (A, B, Result); -- # compute
      PO.Release;
      return Result; -- # compute
   end;

end;

