package body LX_Controls is

   function MinX2 (X : Natural) return Natural is
      Min : Natural;
      Min_Is_X : exception;

   begin
      Min := 2;
      if X < 2 then
         raise Min_Is_X;  -- # returnX
      end if;
      return Min;         -- # returnMin
   exception
      when Min_Is_X =>
         return X;        -- # returnX
   end;
end;
