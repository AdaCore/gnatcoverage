------------------------------------------------------------------------------
--                                                                          --
--                            GNATcoverage                                  --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
--                                                                          --
------------------------------------------------------------------------------

package Foo
  with Abstract_State => (Data)
is
   procedure Process (Value : Integer);
end Foo;
