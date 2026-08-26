procedure Exempt_Nested is
begin
   --  Nested annotations are ignored, with a warning on the nested Exempt_On
   --  annotations.
   pragma Annotate (Xcov, Exempt_On, "J1");
   null;

   pragma Annotate (Xcov, Exempt_On, "J1.1");
   if True then
      pragma Annotate (Xcov, Exempt_On, "J1.1.1");
      null;
      pragma Annotate (Xcov, Exempt_Off);
   end if;
   pragma Annotate (Xcov, Exempt_Off);

   pragma Annotate (Xcov, Exempt_On, "J1.2");
   if True then
      null;
   end if;
   pragma Annotate (Xcov, Exempt_Off);

   null;
   pragma Annotate (Xcov, Exempt_Off);

   pragma Annotate (Xcov, Exempt_On, "J2");
   null;
   pragma Annotate (Xcov, Exempt_Off);
end Exempt_Nested;
