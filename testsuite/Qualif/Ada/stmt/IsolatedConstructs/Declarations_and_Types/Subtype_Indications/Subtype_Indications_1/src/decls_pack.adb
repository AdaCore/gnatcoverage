package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      subtype My_Var_String is Var_String (X); -- # decl1
      Tmp : My_Var_String;                     -- # decl1
   begin
      Tmp.Data := (others => ' ');             -- # code1
      Res := Tmp.Data'Length in L .. R;        -- # code1
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      subtype My_Range is Integer range L .. R; -- # decl2
   begin
      return X in My_Range;                     -- # code2
   end Local_2;

end Decls_Pack;
