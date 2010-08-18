package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      Tmp : array (Integer range L .. R) of Integer; -- # decl1
   begin
      Tmp := (others => 1);                          -- # code1
      Res := X in Tmp'Range;                         -- # code1
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      Tmp : array (Integer range L .. R) of Integer; -- # decl2
   begin
      Tmp := (others => 1);                          -- # code2
      return X in Tmp'Range;                         -- # code2
   end Local_2;

end Decls_Pack;
