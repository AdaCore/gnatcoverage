function MinX2 (X : Natural) return Natural is
   M : Integer;
begin
   if X > 2 then    -- # Call
      goto M_Is_2;  -- # MinIs2
   end if;

   M := X;     -- # MinIsX
   return M;   -- # MinIsX

<<M_Is_2>>
    M := 2;    -- # MinIs2
    return M;  -- # MinIs2
end;
