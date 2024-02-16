package body Values is

   procedure Touch (E : in out Object) is
   begin
      E.X := E.X + 1;
   end;

end;
