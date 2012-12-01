function MinX2 (X : Natural) return Natural is
   M : Integer; -- # decl
begin
   if X > 2 then    -- # common
      goto M_Is_2;  -- # xgt2
   end if;

   M := X;     -- # xle2
   return M;   -- # xle2

<<M_Is_2>>
    M := 2;    -- # xgt2
    return M;  -- # xgt2
end;
