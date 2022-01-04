------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2022, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Support, Robot; use Support, Robot;

procedure Test_Clear_Step is
begin
   Assert (Eval (Cmd => Step, Obs => None) = Safe);
end Test_Clear_Step;
