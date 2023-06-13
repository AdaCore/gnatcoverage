procedure Foo is
   function Fact (I : Integer) return Integer is
   begin
      if I <= 1 then
         return 1;
      else
         return I * Fact (I - 1);
      end if;
   end Fact;
begin
   if Fact (6) /= 0 then
      raise Program_Error;
   end if;
end Foo;
