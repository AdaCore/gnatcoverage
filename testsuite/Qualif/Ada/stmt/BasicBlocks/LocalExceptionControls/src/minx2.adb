function MinX2 (X : Natural) return Natural is
   Min : Natural;
   Min_Is_X : exception;

begin
   Min := 2;           -- # Call
   if X < 2 then       -- # Call
      raise Min_Is_X;  -- # MinIsX
   end if;
   return Min;         -- # MinIs2
exception
   when Min_Is_X =>
      return X;        -- # MinIsX
end;
