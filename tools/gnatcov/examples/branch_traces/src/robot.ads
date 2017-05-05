------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package Robot is

   type Command is (Step, Wait);
   type Obstacle is (Rock, Pit, None);

   type Status is (Safe, Unsafe);

   function Eval (Cmd : Command; Obs : Obstacle) return Status;
   --  How is it to run command CMD in front of obstacle OBS ?

end Robot;
