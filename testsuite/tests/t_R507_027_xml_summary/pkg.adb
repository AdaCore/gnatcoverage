package body Pkg is

   function F1 (I : Integer) return Boolean is
   begin
      if I < 0 then
         return False;
      else
         return True;
      end if;
   end F1;

   function F2 (I : Integer) return Boolean is
   begin
      pragma Annotate (Xcov, Exempt_On, "Exempted because this is a test");
      if I > 10 then
         return False;
      else
         return True;
      end if;
      pragma Annotate (Xcov, Exempt_Off);
   end F2;

end Pkg;
