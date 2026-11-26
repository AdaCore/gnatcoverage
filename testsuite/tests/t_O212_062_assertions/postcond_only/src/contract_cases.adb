package body Contract_Cases is

    function F (I : Integer) return Integer is
    begin
       return I / 2;
    end F;

   function F2 (I : Integer) return Integer is
   begin
      if I < 0 then
         return 0;
      else
         return 1;
      end if;
   end F2;

end Contract_Cases;
