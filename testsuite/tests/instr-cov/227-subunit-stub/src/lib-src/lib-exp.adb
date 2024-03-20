separate (Lib)
function Exp (Left, Right : Positive) return Positive is
begin
   if Right = 1 then
      return Left;
   elsif Right mod 2 = 0 then
      return Exp (Mult (Left, Left), Right / 2);
   else
      return Mult (Left, Exp (Mult (Left, Left), Right / 2));
   end if;
end Exp;
