separate (Lib)
function Mult (Left, Right : Positive) return Positive is
begin
   if Right = 1 then
      return Left;
   elsif Right mod 2 = 0 then
      return 2 * Mult (Left, Right / 2);
   else
      return Add (2 * Mult (Left, Right / 2), Left);
   end if;
end Mult;
