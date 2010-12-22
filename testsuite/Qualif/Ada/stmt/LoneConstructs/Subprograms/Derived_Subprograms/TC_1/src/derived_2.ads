with Derived_1;
package Derived_2 is
   type Derived_T_2 is new Derived_1.Derived_T_1 with null record;

   function Fun2 (X : Derived_T_2) return Integer;
   procedure Proc2 (X : in out Derived_T_2);

   Derived_T_2_Zero : constant Derived_T_2 := (I => 0);

   type Rec2 is record
      Comp : Integer := Fun2 (Derived_T_2_Zero);
   end record;

   type Access_DT2_Proc is access procedure (X : in out Derived_T_2);

end Derived_2;
