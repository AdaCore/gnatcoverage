pragma Ada_2012;
package Andthen is
   
   pragma Elaborate_Body; -- # decl
   -- Just because the testcase architecture expects to always have a body
   
   function And_Then (A, B : Boolean) return Boolean is
      (A and then B);  -- # evalStmt :o/e:
end;
