pragma Ada_2012;
package Andporp is
   pragma Elaborate_Body; -- # decl
   -- Just because the testcase architecture expects to always have a body
   
   function F (A, B, C : Boolean) return Boolean is
      (A and then (B or else C));  -- # evalStmt :o/e:
end;
