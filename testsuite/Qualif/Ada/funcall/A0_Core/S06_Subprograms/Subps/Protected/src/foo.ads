pragma Ada_2012;

package Foo is

   protected type Obj_Type is
      function F return Integer;
      function E (I : Integer) return Boolean;
      procedure P (S : String);
      procedure NP;

   end Obj_Type;

   Obj : Obj_Type; -- # decl

end Foo;
