package body Services is
   function GT (X, Y : Int) return Boolean is
   begin
      return X.Value > Y.Value;
   end;
   function EQ (X, Y : Int) return Boolean is
   begin
      return X.Value = Y.Value;
   end;
   function GE (X, Y : Int) return Boolean is
   begin
      return GT (X, Y) or else EQ (X, Y);
   end;
end;
