package body Pkg is

   function Is_Space (C : Character) return Boolean is
   begin
      if C = ' ' then
         return True;

      --  If gnatcov instrument wrongly propagates `pragma Ada_2012` from
      --  the spec to this body, it will create an if-expression here.
      --  This would make the instrumented sources fail to compile when gnat
      --  is invoked with -gnat05 or lower.

      elsif C = ASCII.HT then

         return True;
      end if;

      return False;
   end Is_Space;

end Pkg;
