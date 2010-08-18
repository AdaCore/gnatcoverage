package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      Tmp : Vector (L .. R);         -- # decl1
   begin
      Tmp := (others => 1);          -- # code1
      Res := Tmp'Length in L .. R;   -- # code1
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      Tmp : Integer range L .. R;    -- # decl2
   begin
      Tmp := L;                      -- # code2
      return X in Tmp .. R;          -- # code2
   end Local_2;

end Decls_Pack;
