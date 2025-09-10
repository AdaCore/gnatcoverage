package body Usedef is
   function Make_Custom (X : Boolean) return R is
   begin
      return (Comp => Customdef.Custom (X));
   end Make_Custom;
end Usedef;
