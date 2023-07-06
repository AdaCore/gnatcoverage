package Procedure_Under_Test is

   function Calculations (A : Integer; B : Integer) return Integer;
   pragma Import
      (Convention    => C,
       Entity        => Calculations,
       External_Name => "calculations");

   function Calculations_Problematic (A : Integer; B : Integer) return Integer;
   pragma Import
      (Convention    => C,
       Entity        => Calculations_Problematic,
       External_Name => "calculations_problematic");

   procedure Test
     (Control : Integer;
      In_A    : Integer;
      In_B    : Integer);

end Procedure_Under_Test;
