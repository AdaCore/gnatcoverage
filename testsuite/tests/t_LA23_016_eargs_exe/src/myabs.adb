function Myabs (X : Integer) return Integer is
begin
   if X < 0 then
      return -X;
   else
      return X;
   end if;
end;
