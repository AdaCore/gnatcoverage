function CheckX (X, Lb, Hb : Integer; Docheck : Boolean) return Boolean is
begin
   return Docheck and then X in Lb .. Hb;
end;
