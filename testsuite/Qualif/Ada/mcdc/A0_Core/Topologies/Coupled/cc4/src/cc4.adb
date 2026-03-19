function CC4 (A, B, C : Boolean) return Boolean is
begin
   return (A and then B) or else (not A and then C);  -- # eval
end;
