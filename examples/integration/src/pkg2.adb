------------------------------------------------------------------------------
--                                                                          --
--                              GNATcoverage                                --
--                                                                          --
--                    Copyright (C) 2021-2021, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Pkg1; use Pkg1;

package body Pkg2 is

   function Add0 (X : Integer) return Integer is
   begin
      return X;
   end Add0;

   function Add3 (X : Integer) return Integer is
   begin
      return Add1 (Add2 (X));
   end Add3;

end Pkg2;
