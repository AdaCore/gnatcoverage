
package body Ops is

   protected type Opmaster is
      procedure Set_Op (O : Opkind);
      procedure Compute (A, B : Boolean; Result : out Boolean);

   private
      Op : Opkind;  -- # decl
   end Opmaster;

   protected body Opmaster is
      procedure Set_Op (O : Opkind) is
      begin
         Op := O; -- # compute
      end Set_Op;

      procedure Compute (A, B : Boolean; Result : out Boolean) is
      begin
         case Op is -- # compute
            when Op_And => Result := A and then B; -- # do_and
            when Op_Or  => Result := A or else B;  -- # do_or
         end case;
      end Compute;
   end Opmaster;

   function Compute (Op : Opkind; A, B : Boolean) return Boolean is
      PO : Opmaster;
      Result : Boolean;
   begin
      PO.Set_Op (Op); -- # compute
      PO.Compute (A, B, Result); -- # compute
      return Result; -- # compute
   end;

end;
