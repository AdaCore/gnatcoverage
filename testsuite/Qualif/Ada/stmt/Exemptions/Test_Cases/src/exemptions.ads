with Support; use Support;
package Exemptions is

   procedure Swap (I, J : in out Integer);
   --   Body contains exemption section for a declaration

   function Factorial (X : Natural) return Positive;
   --  Body contains exemption section for a statement

   procedure Another_Swap (I, J : in out Integer) ;
   --  No exemption section in the body

   function Another_Factorial (X : Natural) return Positive;
   --  No exemption section in the body

   X : Integer := Identity (1);                           -- # decl

   pragma Annotate                                         -- # xdecl
     (Xcov, Exempt_On, "exemption on global declaration"); -- # xdecl
   Y : Integer := Identity (2);                            -- # xdecl
   pragma Annotate (Xcov, Exempt_Off);                     -- # xdecl

   Z : Integer := 1;
end Exemptions;
