package body Levels.NOF is

   --  Implement N_Of using a for statement

   function N_Of (L : Level; S : Sample) return Natural is
      N : Natural := 0;        -- # decl
   begin
      for I in S'Range loop    -- # loopExit
         if S (I) = L then     -- # loopBody
            N := N + 1;        -- # incN
         end if;
      end loop;
      return N;                -- # retN
   end;

end;
