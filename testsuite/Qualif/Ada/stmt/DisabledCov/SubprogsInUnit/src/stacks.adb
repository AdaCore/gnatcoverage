package body Stacks is

   --  Single disable coverage region for a couple of entire subprogram
   --  definitions.

   pragma Annotate                                           -- # disabled
     (Xcov, Cov_Off, "we only care about push and pop");     -- # disabled
                                                             -- # disabled
   procedure On                                              -- # disabled
     (S : in out Stack; Op : Op_Kind; V : in out Integer)    -- # disabled
   is                                                        -- # disabled
   begin                                                     -- # disabled
      case Op is                                             -- # disabled
         when Push =>                                        -- # disabled
            if S.Vcount = S.Size then                        -- # disabled
               raise Constraint_Error;                       -- # disabled
            end if;                                          -- # disabled
                                                             -- # disabled
            S.Vcount := S.Vcount + 1;                        -- # disabled
            S.Values (S.Vcount) := V;                        -- # disabled
                                                             -- # disabled
         when Pop =>                                         -- # disabled
            if S.Vcount = 0 then                             -- # disabled
               raise Constraint_Error;                       -- # disabled
            end if;                                          -- # disabled
                                                             -- # disabled
            V := S.Values (S.Vcount);                        -- # disabled
            S.Vcount := S.Vcount - 1;                        -- # disabled
      end case;                                              -- # disabled
   exception                                                 -- # disabled
      when Constraint_Error =>                               -- # disabled
         S.Ecount := S.Ecount + 1;                           -- # disabled
   end;                                                      -- # disabled
                                                             -- # disabled
   function  Errcount (S : in Stack) return Natural is       -- # disabled
   begin                                                     -- # disabled
      return S.Ecount;                                       -- # disabled
   end;                                                      -- # disabled
                                                             -- # disabled
   pragma Annotate  (Xcov, Cov_On);                          -- # disabled

   --  Then a couple of subprograms without any disable coverage region

   procedure Push (S : in out Stack; Value : Integer) is
      V : Integer := Value;       -- # push_decl
   begin
      On (S, Op => Push, V => V); -- # push_body
   end;

   procedure Pop (S : in out Stack; Value : out Integer) is
      V : Integer := 0;          -- # pop_decl
   begin
      On (S, Op => Pop, V => V); -- # pop_body
      Value := V;                -- # pop_body
   end;

end;
