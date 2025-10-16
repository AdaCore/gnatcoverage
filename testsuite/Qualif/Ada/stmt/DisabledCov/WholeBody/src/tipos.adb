function Tipos (X : Integer) return Integer is
begin
   --  Full sequence exempted below

   pragma Annotate                                    -- # disabled
      (Xcov, Cov_Off, "test disabling all coverage"); -- # disabled
   return X;                                          -- # disabled
   pragma Annotate (Xcov, Cov_On);                    -- # disabled
end;
