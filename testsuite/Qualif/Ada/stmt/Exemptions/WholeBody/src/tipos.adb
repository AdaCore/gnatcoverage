function Tipos (X : Integer) return Integer is
begin
   --  Full sequence exempted below

   pragma Annotate (Xcov, Exempt_On,       -- # xblock
                   "test exempting all");  -- # xblock
   if X > 0 then                           -- # xblock
      return X * 2;                        -- # xblock
   else                                    -- # xblock
      return X;                            -- # xblock
   end if;                                 -- # xblock
   pragma Annotate (Xcov, Exempt_Off);     -- # xblock
end;
