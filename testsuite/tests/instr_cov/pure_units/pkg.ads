pragma Ada_2012;

package Pkg is

   pragma Pure;

   Param_1      : constant Boolean with Import, External_Name => "param_1";
   Param_2      : constant Boolean with Import, External_Name => "param_2";
   Some_Setting : constant Boolean := Param_1 and then Param_2;

   function Fact (I : Integer) return Integer;

   type Stuff is null record;

   procedure Do_Nothing_1 is null;
   procedure Do_Nothing_2 is null;

   function Identity (B : Boolean) return Boolean is (B);
   function Opposite (B : Boolean) return Boolean is (not B);

end Pkg;
