pragma Ada_2012;

with Expr; use Expr;

function Forall (S : Sequence; P : Predicate) return Boolean is
begin
   if (for all X of S => P (X)) then -- # eval
      return True; -- # true
   else
      return False; -- # false
   end if;
end;
