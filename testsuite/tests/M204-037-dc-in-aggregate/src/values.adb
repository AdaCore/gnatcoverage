package body Values is
   
   procedure Equal (X, Y : Integer; B : out Bool) is
   begin
      B := (Value => X = Y); -- # eq
   end;
   
   procedure Equal2 (X, Y : Integer; Z, T : Integer; B : out Bool) is
   begin
      B := (Value => (X = Y) and then (Z = T));  -- # andthen
   end;
   
end;
