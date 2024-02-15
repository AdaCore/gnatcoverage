pragma Ada_2012;
pragma Assertion_Policy (Post => Check);

package body Values is

   function CheckedPlus (A, B : Integer) return Integer
     with Post => (CheckedPlus'Result > 0) -- # eval
   is
   begin
      return A + B; -- # stmt
   end;

   function Plus (A, B : Integer) return Integer is
   begin
      return CheckedPlus (A, B); -- # stmt
   end;
end;
