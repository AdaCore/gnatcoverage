pragma Ada_12;
pragma Assertion_Policy (Static_Predicate => Disable);

package Ops is
   
   type T_Day is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
   
   subtype Early_Day is T_Day with
     Static_Predicate => Early_Day in Monday | Tuesday | Wednesday; -- # check-early
   
   subtype Mid_Day is T_Day with
     Static_Predicate => Mid_Day = Thursday; -- # check-mid
   
   subtype Late_Day is T_Day with
     Static_Predicate => Late_Day in Friday | Saturday | Sunday; -- # check-late
      
   type T_Wpos is (Early, Mid, Late);
   function Wpos (D : T_Day) return T_Wpos;
   
   procedure Check_Early (D : T_Day);
   procedure Check_Mid (D : T_Day);
   procedure Check_Late (D : T_Day);
end;
