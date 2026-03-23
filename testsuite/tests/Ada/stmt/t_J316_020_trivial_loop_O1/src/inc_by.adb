procedure Inc_By (N : Natural; X : in out Integer) is
   Ninc : Natural := 0;  -- # decl
begin -- #
   while Ninc /= N loop  -- # incLoop
      X := X + 1;        -- # incX
      Ninc := Ninc + 1;  -- # incX
   end loop;
end;  -- #
