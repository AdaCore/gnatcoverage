function Porpand (A, B, C : Boolean) return Boolean is
begin
   return (A or else B) and then C; -- # eval
end;

