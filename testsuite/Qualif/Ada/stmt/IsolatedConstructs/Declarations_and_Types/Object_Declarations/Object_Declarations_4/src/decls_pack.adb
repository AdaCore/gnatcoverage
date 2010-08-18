package body Decls_Pack is

   procedure Local_1 (Res : in out Boolean) is
      T_Private_V : T_Private := T_Private_Zero;   -- # decl1
   begin
      Res := not Res;                              -- # code1
   end Local_1;

   function Local_2 (Arg : Boolean) return Boolean is
      T_Private_V : T_Private;                     -- # decl2
   begin
      return not Arg;                              -- # code2
   end Local_2;

end Decls_Pack;
