
package Vectors is
   
   --  This unit is devised to expose services making use of unconstrained
   --  array types requiring secondary stack allocations for oject declarations.
   
   type Vector is array (Natural range <>) of Integer;
   
   function Sum_All_Abs (N : Integer; Value : Integer) return Integer;
end;
