function MinX2 (X : Natural) return Natural is
   Min : Natural;      -- # decl
   Xle2 : exception;   -- # decl

begin
   Min := 2;           -- # common
   if X <= 2 then      -- # common
      raise Xle2;      -- # xle2
   end if;
   return Min;         -- # xgt2
exception
   when Xle2 =>
      return X;        -- # xle2
end;
