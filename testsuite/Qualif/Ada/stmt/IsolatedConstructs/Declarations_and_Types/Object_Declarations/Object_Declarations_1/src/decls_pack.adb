package body Decls_Pack is

   procedure Local_1 (Res : in out Boolean) is
      F : Float := 0.0;                    -- # decl1
   begin
      Res := not Res;                      -- # code1
   end Local_1;

   function Local_2 (Arg : Boolean) return Boolean is
      Day : Week_Day := Sun;               -- # decl2
   begin
      return not Arg;                      -- # code2
   end Local_2;

end Decls_Pack;
