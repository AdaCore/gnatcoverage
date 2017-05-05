------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Support, Robot; use Support, Robot;

procedure Test_Rock_Wait is
begin
   Assert (Eval (Cmd => Wait, Obs => Rock) = Safe);
end Test_Rock_Wait;
