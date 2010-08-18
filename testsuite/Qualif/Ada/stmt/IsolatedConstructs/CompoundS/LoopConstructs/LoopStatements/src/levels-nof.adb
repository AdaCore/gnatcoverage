package body Levels.NOF is

   --  Implement N_Of using loop/exit constructs. Single exit.

   function N_Of (L : Level; S : Sample) return Natural is
      I : Integer := S'First;  -- # decl
      N : Natural := 0;        -- # decl
   begin
      loop
         exit when I > S'Last; -- # loopExit
         if S (I) = L then     -- # loopBody
            N := N + 1;        -- # incN
         end if;
         I := I + 1;           -- # loopBody
      end loop;
      return N;                -- # retN
   end;

end;
