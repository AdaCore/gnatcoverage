package body Pkg is

   function Fact (I : Integer; B : Boolean) return Integer is
   begin
      if I < 2 and then B then
         return 1;
      else
         return I * Fact (I - 1, B);
      end if;
   end Fact;

end Pkg;
