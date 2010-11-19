package body Stacks is

   pragma Annotate                                           -- # xregion
     (Xcov, Exempt_On, "we only care about push and pop");   -- # xregion
                                                             -- # xregion
   procedure On                                              -- # xregion
     (S : in out Stack; Op : Op_Kind; V : in out Integer)    -- # xregion
   is                                                        -- # xregion
   begin                                                     -- # xregion
      case Op is                                             -- # xregion
         when Push =>                                        -- # xregion
            if S.Vcount = S.Size then                        -- # xregion
               raise Constraint_Error;                       -- # xregion
            end if;                                          -- # xregion
                                                             -- # xregion
            S.Vcount := S.Vcount + 1;                        -- # xregion
            S.Values (S.Vcount) := V;                        -- # xregion
                                                             -- # xregion
         when Pop =>                                         -- # xregion
            if S.Vcount = 0 then                             -- # xregion
               raise Constraint_Error;                       -- # xregion
            end if;                                          -- # xregion
                                                             -- # xregion
            V := S.Values (S.Vcount);                        -- # xregion
            S.Vcount := S.Vcount - 1;                        -- # xregion
      end case;                                              -- # xregion
   exception                                                 -- # xregion
      when Constraint_Error =>                               -- # xregion
         S.Ecount := S.Ecount + 1;                           -- # xregion
   end;                                                      -- # xregion
                                                             -- # xregion
   function  Errcount (S : in Stack) return Natural is       -- # xregion
   begin                                                     -- # xregion
      return S.Ecount;                                       -- # xregion
   end;                                                      -- # xregion
                                                             -- # xregion
   pragma Annotate  (Xcov, Exempt_Off);                      -- # xregion

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
