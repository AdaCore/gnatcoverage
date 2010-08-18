function MinX2 (X : Natural) return Natural is
   M : Integer;
begin
   loop
      M := 0;           -- # x0
      exit when X = 0;  -- # x0

      M := 1;           -- # x1
      exit when X = 1;  -- # x1

      M := 2;           -- # x2
      exit when X = 2;  -- # x2

      exit when X /= 2; -- # xnot2
   end loop;

   return M;            -- # return
end;
