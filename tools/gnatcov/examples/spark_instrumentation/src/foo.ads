------------------------------------------------------------------------------
--                                                                          --
--                            GNATcoverage                                  --
--                                                                          --
--                    Copyright (C) 2008-2022, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package Foo
   with Abstract_State => (Data)
is
   procedure Process (Value : Integer);
end Foo;
