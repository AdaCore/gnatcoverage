generic
   type T is new Float;
   Low, High : T;
package Check_G is
   procedure Test (Value : T);
   pragma Inline (Test);

   In_Range_Count     : Natural := 0;
   Out_Of_Range_Count : Natural := 0;
end Check_G;
