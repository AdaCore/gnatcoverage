procedure Hello is
begin
   null;
   pragma Annotate (Xcov, Exempt_Off, "BAD lone at start");

   pragma Annotate (Xcov, Exempt_On, "OK");
   pragma Annotate (Xcov, Exempt_Off, "OK");

   pragma Annotate (Xcov, Exempt_On, "OK2");
   pragma Annotate (Xcov, Exempt_Off, "");

   pragma Annotate (Xcov, Exempt_On, "OK3");
   pragma Annotate (Xcov, Exempt_Off);

   pragma Annotate (Xcov, Exempt_On);
   pragma Annotate (Xcov, Exempt_Off);

   pragma Annotate (Xcov, Exempt_On, "BAD1");
   pragma Annotate (Xcov, Exempt_Off, "BAD1 mismatch");

   pragma Annotate (Xcov, Exempt_On, "BAD2");
   pragma Annotate (Xcov, Exempt_On,  "BAD2 nested");
   pragma Annotate (Xcov, Exempt_Off, "BAD2 nested");
   pragma Annotate (Xcov, Exempt_Off, "BAD2");

   pragma Annotate (Xcov, Exempt_On, "BAD lone at end");

   pragma Annotate (Xcov);
   pragma Annotate (Xcov, Exempt_Bad1);
   pragma Annotate (Xcov, Exempt_Bad2, "foo");
end Hello;
