procedure Dis (X : Integer) is
begin
   pragma Annotate (Xcov, Cov_Off, "not of interest");
   if X = 42 then
      null;
   end if;
   pragma Annotate (Xcov, Cov_On);
end Dis;
