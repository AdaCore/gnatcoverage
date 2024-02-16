with State; use State;

-- A few items of note here:
--
-- * The X > 0 test can be always True (or False) for some definitions
--   of Value_T. The compiler would normally optimize this away in instances
--   on request (-O1), even with -fpreserve-control-flow.
--
-- * The tests on formal "in" parameters are always known at
--   at compiler time for a given instance, and the compiler
--   takes advantage of that to simplify early, even at O0.

package body Values is
   function F (X : in Value_T) return Integer Is
      Adjusted_Factor : Integer;
      V : Integer;
   begin
      if X > 0 then -- # test_x
         V := Integer (X + 1);    -- # xpos
      else
         V := Integer (X - 4);   -- # xneg
      end if;

      if Factor > 0 then -- # test_factor
         Adjusted_Factor := Factor * 2; -- # fpos
      else
         Adjusted_Factor := Factor; -- # fneg
      end if;

      return V + Adjusted_Factor; -- # stmt
   end;
end;
