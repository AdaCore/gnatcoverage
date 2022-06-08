pragma Ada_2012;

with Expr; use Expr;

function Forall (S : Sequence; P : Predicate) return Boolean is
begin
   if (for all I in S'Range => P (S(I))) then -- # eval
      return True; -- # true
   else
      return False; -- # false
   end if;
end;
