function Tipos (X : Integer) return Integer is
begin
   --  Full sequence exempted below

   pragma Annotate (Xxxx, Exempt_On,       -- # xblock
                   "test exempting all");  -- # xblock
   if X > 0 then                           -- # xblock_if
      return X * 2;                        -- # xblock_r1
   else                                    -- # xblock
      return X;                            -- # xblock_r2
   end if;                                 -- # xblock
   pragma Annotate (Xxxx, Exempt_Off);     -- # xblock
end;
