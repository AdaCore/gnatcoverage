------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Orand; use Support;

procedure Test_Orand  is
   X : constant Boolean := True;
begin
   Assert (Orand (True, X, True) = True);
   Assert (Orand (False, False, X) = False);
   Assert (Orand (False, True, False) = False);
end Test_Orand;
