function Orelse (A, B : Boolean) return Boolean is
begin
   return A      -- # eval0 :o/e:
     or else B;  -- # eval1
end;
