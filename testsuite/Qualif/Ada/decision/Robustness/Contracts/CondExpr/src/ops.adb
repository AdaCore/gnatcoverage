package body Ops is
   procedure Add_Or_Mult (X, Y : Integer; Z : in out Integer) is
   begin
      if X <= 3 then  -- # test
         Z := X + Y;  -- # add
      else
         Z := X * Y;  -- # mult
      end if;
   end;
end;
