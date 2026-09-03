procedure Cov_Off_Nested is
begin
   --  Nested annotations are ignored, with a warning on the nested Cov_Off
   --  annotations.
   pragma Annotate (Xcov, Cov_Off, "J1");
   null;

   pragma Annotate (Xcov, Cov_Off, "J1.1");
   if True then
      pragma Annotate (Xcov, Cov_Off, "J1.1.1");
      null;
      pragma Annotate (Xcov, Cov_On);
   end if;
   pragma Annotate (Xcov, Cov_On);

   pragma Annotate (Xcov, Cov_Off, "J1.2");
   if True then
      null;
   end if;
   pragma Annotate (Xcov, Cov_On);

   null;
   pragma Annotate (Xcov, Cov_On);

   pragma Annotate (Xcov, Cov_Off, "J2");
   null;
   pragma Annotate (Xcov, Cov_On);
end Cov_Off_Nested;
