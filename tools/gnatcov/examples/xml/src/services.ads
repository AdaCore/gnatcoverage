------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
--                                                                          --
------------------------------------------------------------------------------

package Services is
   function Andthen (A, B : Boolean) return Boolean;
   function Orelse (A, B : Boolean) return Boolean;

   function Oor (A, B : Boolean) return Boolean;
end Services;
