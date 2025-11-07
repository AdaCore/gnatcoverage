pragma Ada_2012;

with Pkg2.Child;

package Pkg is
   type My_Tagged_Rec is tagged null record;
   function Foo (Self : My_Tagged_Rec; Id : Pkg2.Child.My_Id) return Boolean;

private
   use Pkg2.Child;

   function Foo (Self : My_Tagged_Rec; Id : My_Id) return Boolean is (Id > 0 and then Id < 2);

end Pkg;
