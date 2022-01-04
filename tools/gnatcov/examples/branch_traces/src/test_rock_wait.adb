------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2022, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Support, Robot; use Support, Robot;

procedure Test_Rock_Wait is
begin
   Assert (Eval (Cmd => Wait, Obs => Rock) = Safe);
end Test_Rock_Wait;
