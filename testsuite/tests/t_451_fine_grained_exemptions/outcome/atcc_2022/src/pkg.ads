pragma Ada_2022;
pragma Assertion_Policy (Check);

package Pkg is
   pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "never true");
   procedure Print_If (C1, C2 : Boolean; Message : String)
   with Pre => C1 or else C2;  -- # precondition
end Pkg;
