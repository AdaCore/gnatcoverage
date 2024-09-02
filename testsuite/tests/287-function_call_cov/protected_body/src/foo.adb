pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Foo is

   protected body Obj_Type is

      function F return Integer is                       -- # fun
      begin
         return 8;                                       -- # stmt
      end F;

      function E (I : Integer) return Boolean is (True); -- # expr_fun

      procedure P (S : String) is                        -- # fun
      begin
         Put_Line (S);                                   -- # call_stmt
         null;                                           -- # stmt
      end P;

      procedure NP is                                     -- # fun
         null;                                           -- # stmt
   end Obj_Type;

end Foo;
