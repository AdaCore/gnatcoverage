package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      Tmp : Var_String (X);               -- # decl1
   begin
      Tmp.Data := (others => ' ');        -- # code1
      Res := Tmp.Data'Length in L .. R;   -- # code1
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      Tmp : Var_String (X);               -- # decl2
   begin
      Tmp.Data := (others => ' ');        -- # code2
      return Tmp.Data'Length in L .. R;   -- # code2
   end Local_2;

end Decls_Pack;
