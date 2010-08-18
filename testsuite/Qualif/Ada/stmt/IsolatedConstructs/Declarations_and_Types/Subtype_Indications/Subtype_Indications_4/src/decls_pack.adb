package body Decls_Pack is

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer)
   is
      type Arr is array (Week_Day) of Integer range L .. R; -- # decl1
   begin
      declare
         Tmp : Arr := (Mon .. Fri => L, Sat .. Sun => R);   -- # code1
      begin
         Res := X in  Tmp (Tmp'First) .. Tmp (Tmp'Last);    -- # code1
      end;
   end Local_1;

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean
   is
      type Arr is array (Week_Day) of Integer range L .. R; -- # decl2
   begin
      declare
         Tmp : Arr := (Mon .. Fri => L, Sat .. Sun => R);   -- # code2
      begin
         return X in  Tmp (Tmp'First) .. Tmp (Tmp'Last);    -- # code2
      end;
   end Local_2;

end Decls_Pack;
