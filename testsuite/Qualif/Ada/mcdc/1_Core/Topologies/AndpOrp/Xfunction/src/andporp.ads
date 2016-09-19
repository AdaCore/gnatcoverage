pragma Ada_2012;
package Andporp is
   pragma Elaborate_Body; -- # decl
   -- Just because the testcase architecture expects to always have a body
   
   function F (A, B, C : Boolean) return Boolean is -- # xf-profile
      (A and then (B or else C));  -- # evalOther :o/e:
end;
