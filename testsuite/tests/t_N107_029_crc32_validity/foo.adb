procedure Foo is
   function Fact (N : Natural) return Natural is
   begin
      if N <= 1 then
         return 1;
      else
         return N * Fact (N - 1);
      end if;
   end Fact;
begin
   if Fact (6) /= 720 then
      raise Program_Error;
   end if;
end Foo;
