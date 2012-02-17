------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Divmod; use Support;

procedure Test_Divmod0  is
   Value : Integer;
   Divides : Boolean;
begin
   Divmod (X => 5, Y => 0, Value => Value,
           Divides => Divides, Tell => True);
end Test_Divmod0;
