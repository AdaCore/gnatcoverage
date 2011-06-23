--  The body contains "executable" pragmas in different contexts

package Pragmas is


   subtype Acceptable_Integer is Integer range -20_000 .. 20_000;
   subtype Safe_Integer       is Integer range -10_000 .. 10_000;

   function In_Range (X, L, R : Integer) return Boolean;
   --  This function checks whether X is in range L .. R. Creates context where
   --  pragmas may be executed

   function Is_Safe (I : Integer) return Boolean;
   --  Checks if I is in Safe_Integer. Calls procedure with Pre- and
   --  Postcondition pragmas

end Pragmas;
