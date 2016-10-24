package body Ops is
   
   procedure Set (P : in out T_Pair'Class; X, Y : Integer) is
   begin
      P.X := X; -- # stmt
      P.Y := Y; -- # stmt
      P.Is_Set := True; -- # stmt
   end;
   
   function Valid (D : T_Double) return Boolean is
   begin
      return D.Y = D.X * 2; -- # check
   end;
   
   function Bad_Set (D : T_Double) return Boolean is
   begin
      return D.Is_Set and then D.Y /= D.X * 2; -- # eval
   end;
end;
