with Variants_Support;
package Variants is

   type Variant_Record (D : Integer) is record
      case D is
         when 1 =>
            C1 : Integer := Variants_Support.F1 (1);
         when 2 =>
            C2 : Integer := Variants_Support.F2 (2);
         when 3 =>
            C3 : Integer := Variants_Support.F3 (3);
         when others =>
            C_Others : Integer := Variants_Support.F_Others (D);
      end case;
   end record;

end Variants;
