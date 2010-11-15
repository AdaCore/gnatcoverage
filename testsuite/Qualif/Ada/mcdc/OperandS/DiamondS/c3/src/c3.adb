function C3 (A, B, C : Boolean) return Boolean is
begin
   return (A and then B) or else C;  -- # eval
end;
