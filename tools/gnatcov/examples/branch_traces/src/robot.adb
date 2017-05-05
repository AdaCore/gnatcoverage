------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package body Robot is

   ----------
   -- Eval --
   ----------

   function Eval (Cmd : Command; Obs : Obstacle) return Status is
   begin
      --  Stepping forward into a rock or into a pit is unsafe

      if (Obs = Pit or else Obs = Rock) and then Cmd = Step then
         return Unsafe;
      else
         return Safe;
      end if;
   end Eval;
end Robot;
