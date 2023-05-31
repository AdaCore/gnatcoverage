package body Stacks is

   -- Single exemption region for a couple of entire subprogram definitions

   pragma Annotate                                           -- # xregion
     (Xcov, Exempt_On, "we only care about push and pop");   -- # xregion
                                                             -- # xregion
   procedure On                                              -- # xregion
     (S : in out Stack; Op : Op_Kind; V : in out Integer)    -- # xregion
   is                                                        -- # xregion
   begin                                                     -- # xregion
      case Op is                                             -- # xregion_01
         when Push =>                                        -- # xregion
            if S.Vcount = S.Size then                        -- # xregion_02
               raise Constraint_Error;                       -- # xregion_03
            end if;                                          -- # xregion
                                                             -- # xregion
            S.Vcount := S.Vcount + 1;                        -- # xregion_04
            S.Values (S.Vcount) := V;                        -- # xregion_05
                                                             -- # xregion
         when Pop =>                                         -- # xregion
            if S.Vcount = 0 then                             -- # xregion_06
               raise Constraint_Error;                       -- # xregion_07
            end if;                                          -- # xregion
                                                             -- # xregion
            V := S.Values (S.Vcount);                        -- # xregion_08
            S.Vcount := S.Vcount - 1;                        -- # xregion_09
      end case;                                              -- # xregion
   exception                                                 -- # xregion
      when Constraint_Error =>                               -- # xregion
         S.Ecount := S.Ecount + 1;                           -- # xregion_10
   end;                                                      -- # xregion
                                                             -- # xregion
   function  Errcount (S : in Stack) return Natural is       -- # xregion
   begin                                                     -- # xregion
      return S.Ecount;                                       -- # xregion_11
   end;                                                      -- # xregion
                                                             -- # xregion
   pragma Annotate  (Xcov, Exempt_Off);                      -- # xregion

   --  Then a couple of subprograms without any exemption

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
