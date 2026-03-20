package body Mylib is

   function Fact (N : Natural) return Natural is
   begin
      if N < 2 then
         return N;
      else
         return N * Fact (N - 1);
      end if;
   end Fact;

end Mylib;
