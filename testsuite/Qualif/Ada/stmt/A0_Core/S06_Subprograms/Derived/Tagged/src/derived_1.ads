with Subprogram_Pack;
package Derived_1 is
   type Derived_T_1 is new Subprogram_Pack.T with null record;
   procedure Proc2 (X : in out Derived_T_1);

   Derived_T_1_Zero : constant Derived_T_1 := (I => 0);

   type Rec1 is record
      Comp : Integer := Fun1 (Derived_T_1_Zero);
   end record;

   type Access_DT1_Proc is access procedure (X : in out Derived_T_1);

end Derived_1;
