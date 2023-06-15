pragma Ada_2012;

package Foo is

   protected type Obj_Type is
      procedure Set (X : Integer);

      procedure Do_Nothing (X : Integer);

      function Get return Integer;

      function Cond return Boolean;

   private
      Val : Integer := 0;  -- # decl
   end Obj_Type;

   Obj : Obj_Type;

end Foo;