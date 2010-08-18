package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      type Rec is record Comp : Vector (L .. R); end record; -- # decl1
   begin
      declare
         Tmp : Rec := (Comp => (others => 0));  -- # code1
      begin
         Res := X in  Tmp.Comp'Range;           -- # code1
      end;
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      type Rec is record Comp : Vector (L .. R); end record; -- # decl2
   begin
      declare
         Tmp : Rec := (Comp => (others => 0));              -- # code2
      begin
         return X in Tmp.Comp'Range;                        -- # code2
      end;
   end Local_2;

end Decls_Pack;
