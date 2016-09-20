package Values is
   procedure Touch (E : in out Integer);
   procedure Check_Value (E : Integer; V : Integer);
   
   type Array_Type is array (1 .. 8) of Integer;
   VA : Array_Type := (others => 5);
end;
