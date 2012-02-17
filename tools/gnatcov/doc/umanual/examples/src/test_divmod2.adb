------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Divmod; use Support;

procedure Test_Divmod2  is
   Value : Integer;
   Divides : Boolean;
begin
   Divmod (X => 5, Y => 2, Value => Value,
           Divides => Divides, Tell => True);
   Assert (Divides = False);

   Divmod (X => 6, Y => 2, Value => Value,
           Divides => Divides, Tell => True);
   Assert (Divides = True);
end Test_Divmod2;
