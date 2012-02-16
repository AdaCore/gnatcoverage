------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package Commands is

   type Command is (Step, Hold);
   type Perceived is (Room, Rock, Pit);

   function Safe (Cmd : Command; Front : Perceived) return Boolean;
   --  Whether executing CMD is safe with FRONT perceived ahead

   N_Safe, N_Unsafe : Integer := 0;
   --  Count the number of safe/unsafe cases we have evaluated

end Commands;
