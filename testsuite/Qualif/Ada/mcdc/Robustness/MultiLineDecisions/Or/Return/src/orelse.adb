function Orelse (A, B : Boolean) return Boolean is
begin
   return A      -- # eval0
     or else B;  -- # eval1
end;
