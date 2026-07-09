pragma Ada_2022;
pragma Assertion_Policy (Check);

package Pkg is
   pragma Annotate (Xcov, Manual_Decision_Evaluation, True, False, "J");
   procedure Print_If (C1, C2 : Boolean; Message : String)
   with Pre => C1 or else C2;  -- # precondition
end Pkg;
