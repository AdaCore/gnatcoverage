separate (Lib)
function Add (Left, Right : Positive) return Positive is
   Result    : Positive := Left;
   Remainder : Natural := Right;
begin
   while Remainder /= 0 loop
      Result := Result + 1;
      Remainder := Remainder - 1;
   end loop;
   return Result;
end Add;
