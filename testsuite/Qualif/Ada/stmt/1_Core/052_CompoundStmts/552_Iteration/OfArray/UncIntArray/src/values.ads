package Values is
   procedure Touch (E : in out Integer);
   procedure Check_Value (E : Integer; V : Integer);
   
   type Array_Type is array (Natural range <>) of Integer;
   VA : Array_Type := (1 .. 8 => 5);
end;
