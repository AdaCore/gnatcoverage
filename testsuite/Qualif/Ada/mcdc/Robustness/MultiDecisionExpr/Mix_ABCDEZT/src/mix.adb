function Mix (A, B, C, D, E, Z, T : Boolean) return Boolean is
begin
   return (A and then ((B or else C) = ((D and then E) /= (Z or else T)))); -- # eval
end;

   
