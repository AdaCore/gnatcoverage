package body Pkg is

   function Difficult_True (X : Integer) return Boolean is
   begin
      if X mod 10 = 0 then
         return (3 * X - 10) mod 10 = 0;
      else
         return False;
      end if;
   end Difficult_True;

end Pkg;
