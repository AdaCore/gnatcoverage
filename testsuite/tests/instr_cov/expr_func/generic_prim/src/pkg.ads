pragma Ada_2012;

generic
   type T (<>) is private;
   Init : Boolean;
package Pkg is

   type Tagged_Typ is tagged record
      Bool : Boolean;
   end record;

   function Foo (Self : Tagged_Typ) return Boolean is (Self.Bool);  -- # expr

   Obj : constant Tagged_Typ := (Bool => Init);

end Pkg;
