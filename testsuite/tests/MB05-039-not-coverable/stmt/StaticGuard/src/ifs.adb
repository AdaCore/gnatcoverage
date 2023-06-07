with State; use State;

package body IFS is
   
   -- Expose various cases with if statements controled by constant guards,
   --  which are simplified early-on by the compiler front-end.
   
   -- Whether a code-less "if XX then" stmt becomes coverable itself
   -- depends on whether it dominates coverable statements.
   
   procedure If_F_Var_Last is
   begin
      Tick;       -- # reach
      if GF then  -- # out
         Dtick;   -- # out
      end if;
   end;
   
   procedure If_F_Cmp_Last is
   begin
      Tick;              -- # reach
      if GT = False then -- # out
         Dtick;          -- # out
      end if;
   end;
   
   procedure If_F_Var_Not_Last is
   begin
      Tick;     -- # reach
      if GF then  -- # test
         Dtick; -- # out
      end if;
      Tick;     -- # reach
   end;

   procedure If_F_Cmp_Not_Last is
   begin
      Tick;      -- # reach
      if GT = False then -- # test
         Dtick;  -- # out
      end if;
      Tick;      -- # reach
   end;
   
   --
   
   procedure Ifelse_F_Var_Last is
   begin
      Tick;       -- # reach
      if GF then  -- # test
         Dtick;   -- # out
      else
         Tick;    -- # reach
      end if;
   end;
   
   procedure Ifelse_F_Cmp_Last is
   begin
      Tick;       -- # reach
      if GT = False then  -- # test
         Dtick;   -- # out
      else
         Tick;    -- # reach
      end if;
   end;
   
   procedure Ifelse_F_Var_Not_Last is
   begin
      Tick;       -- # reach
      if GF then  -- # test
         Dtick;   -- # out
      else
         Tick;    -- # reach
      end if;
      Tick;              -- # reach
   end;
   
   procedure Ifelse_F_Cmp_Not_Last is
   begin
      Tick;      -- # reach
      if GT = False then  -- # test
         Dtick;  -- # out
      else
         Tick;   -- # reach
      end if;
      Tick;      -- # reach
   end;
   
   --
   
   procedure Ifelse_T_Var_Last is
   begin
      Tick;       -- # reach
      if GT then  -- # test
         Tick;    -- # reach
      else
         Dtick;   -- # out
      end if;
   end;
   
   procedure Ifelse_T_Cmp_Last is
   begin
      Tick;      -- # reach
      if GT = True then  -- # test
         Tick;   -- # reach
      else
         Dtick;  -- # out
      end if;      
   end;
   
   procedure Ifelse_T_Var_Not_Last is
   begin
      Tick;       -- # reach
      if GT then  -- # test
         Tick;    -- # reach
      else
         Dtick;   -- # out
      end if;
      Tick;       -- # reach
   end;
   
   procedure Ifelse_T_Cmp_Not_Last is
   begin
      Tick;      -- # reach
      if GT = True then  -- # test
         Tick;   -- # reach
      else
         Dtick;  -- # out
      end if;
      Tick;      -- # reach
   end;
   
end;
