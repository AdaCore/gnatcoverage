package body Levels.ALT is

   --  Implement All_LT with a while statement.

   function All_LT (L : Level; S : Sample) return Boolean is
      I : Integer;
      Last_LT : Boolean;
      -- Whether the last examined sample value is known to be < L
   begin

      --  Start True, value to return if S is empty, then loop over S elements
      --  as long as none is found not to satisfy the criterion.

      Last_LT := True;  -- # preLoop

      I := S'First;     -- # preLoop
      while I <= S'Last loop        -- # indexCheck
         Last_LT := False;          -- # preValCheck
         exit when not (S (I) < L); -- # valueCheck
         Last_LT := True;           -- # postValCheck
         I := I + 1;                -- # postValCheck
      end loop;

      return Last_LT;    -- # postLoop
   end;
end;
