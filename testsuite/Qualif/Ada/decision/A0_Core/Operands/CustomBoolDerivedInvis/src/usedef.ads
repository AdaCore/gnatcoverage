with Customdef;
package Usedef is
   type R is record
      Comp : Customdef.Custom;
   end record;
   function Make_Custom (X : Boolean) return R;
end Usedef;
