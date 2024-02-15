package Values is
   type Array_Type is array (Natural range <>) of Integer;

   procedure Touch (E : in out Integer);
end;
