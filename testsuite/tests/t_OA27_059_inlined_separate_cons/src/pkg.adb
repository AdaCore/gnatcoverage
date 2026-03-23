package body Pkg is

   function Is_Even (I : Integer) return Boolean is
   begin
      if I mod 2 = 0 then
         return True;
      else
         return False;
      end if;
   end Is_Even;

   procedure Test_Driver is separate;

end Pkg;
