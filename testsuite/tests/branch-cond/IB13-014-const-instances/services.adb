package body Services is
   function Has_Mid_Precision (N : Number) return Boolean is
   begin
      return Number'Digits >= 6 and then Number'Digits <= 12;
   end;
end;
