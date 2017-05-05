------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Support, Robot; use Support, Robot;

procedure Test_Pit_Step is
begin
   Assert (Eval (Cmd => Step, Obs => Pit) = Unsafe);
end Test_Pit_Step;
