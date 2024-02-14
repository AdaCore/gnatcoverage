package body Code is
   function Compute (X : Boolean; Y : Boolean) return Boolean is
   begin
      return X and then Y;
   end Compute;
end Code;
