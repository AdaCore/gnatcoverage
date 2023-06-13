package body Pkg is
   
   function Foo (Var : Unbounded_String) return Boolean is
   begin
      if Var = To_Unbounded_String ("abc")        -- # evalA
        or else Var = To_Unbounded_String ("def") -- # evalB
        or else Var = To_Unbounded_String ("ghi") -- # evalC
      then
         return True;  -- # dtrue
      else
         return False; -- # dfalse
      end if;
   end Foo;
   
end;
