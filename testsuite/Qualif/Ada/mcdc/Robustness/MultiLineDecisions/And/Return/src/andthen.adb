function Andthen (A, B : Boolean) return Boolean is
begin
   return A           -- # eval0
     and then B;      -- # eval1
end;


