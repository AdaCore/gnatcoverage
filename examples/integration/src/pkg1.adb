------------------------------------------------------------------------------
--                                                                          --
--                              GNATcoverage                                --
--                                                                          --
--                    Copyright (C) 2021-2021, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package body Pkg1 is

   function Add1 (X : Integer) return Integer is
   begin
      return X + 1;
   end Add1;

   function Add2 (X : Integer) return Integer is
   begin
      return X + 2;
   end Add2;

end Pkg1;
