procedure Cov_Off_Overlap is
begin
   --  Cov_Off from external annotations here
   pragma Annotate (Xcov, Cov_Off, "JI1");
   null;
   pragma Annotate (Xcov, Cov_On);
   --  Cov_On from external annotations here
   null;

   pragma Annotate (Xcov, Cov_Off, "JI2");
   null;
   --  Cov_Off from external annotations here
   null;
   pragma Annotate (Xcov, Cov_On);
   null;
   --  Cov_On from external annotations here

   null;
   --  Cov_Off from external annotations at the "r" in the "pragma" keyword
   --  just below.
   pragma Annotate (Xcov, Cov_Off, "JI4");
   null;
   --  Cov_On from external annotations here
   null;
   pragma Annotate (Xcov, Cov_On);
   null;

   pragma Annotate (Xcov, Cov_Off, "JI4");
   null;
   --  Cov_Off from external annotations here
   null;
   pragma Annotate (Xcov, Cov_On);
   null;
end Cov_Off_Overlap;
