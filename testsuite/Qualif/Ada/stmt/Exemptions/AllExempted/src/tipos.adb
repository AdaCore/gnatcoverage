function Tipos (X : Integer) return Integer is
begin
   pragma Annotate (Xcov, Exempt_On, "test exempting all");
   if X > 0 then    -- # test
      return X * 2; -- # pos
   else
      return X;     -- # other
   end if;
   pragma Annotate (Xcov, Exempt_Off);
end;
