pragma Ada_2005;

package Pkg is

   type Enum is (A, B, C);

   Enum_Image_A : aliased constant String := "A";
   Enum_Image_B : aliased constant String := "B";
   Enum_Image_C : aliased constant String := "";
   Enum_Images : constant array (Enum) of access constant String :=
      (Enum_Image_A'Access,
       Enum_Image_B'Access,
       Enum_Image_C'Access);

   procedure Dump (E : Enum);

end Pkg;
