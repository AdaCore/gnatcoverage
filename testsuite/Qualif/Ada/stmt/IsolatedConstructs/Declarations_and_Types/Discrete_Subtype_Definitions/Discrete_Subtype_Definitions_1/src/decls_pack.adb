package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      type Arr is array (Integer range L .. R) of Integer; -- # decl1
   begin
      declare
         Tmp : Arr := (others => 1);                       -- # code1
      begin
         Res := X in Tmp'Range;                            -- # code1
      end;
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      type Arr is array (Integer range L .. R) of Integer; -- # decl2
   begin
      declare
         Tmp : Arr := (others => 1);                       -- # code2
      begin
         return X in Tmp'Range;                            -- # code2
      end;
   end Local_2;

end Decls_Pack;
