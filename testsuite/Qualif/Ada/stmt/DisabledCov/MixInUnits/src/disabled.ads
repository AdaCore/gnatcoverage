with Support; use Support;
package Disabled is

   procedure Swap (I, J : in out Integer);
   --   Body contains disabled coverage section for a declaration

   function Factorial (X : Natural) return Positive;
   --  Body contains disabled coverage section for a statement

   procedure Another_Swap (I, J : in out Integer) ;
   --  No disabled coverage section in the body

   function Another_Factorial (X : Natural) return Positive;
   --  No disabled coverage section in the body

   --  Mix of disabled and non-disabled declarations

   X : Integer := Identity (1);                                 -- # decl

   pragma Annotate                                              -- # disabled
     (Xcov, Cov_Off, "disable coverage on global declaration"); -- # disabled
   Y : Integer := Identity (2);                                 -- # disabled
   pragma Annotate (Xcov, Cov_On);                              -- # disabled

   Z : Integer := 1;
end Disabled;
