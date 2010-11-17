package body Stacks is

   procedure On (S : in out Stack; Op : Op_Kind; V : in out Integer) is
   begin
      case Op is                        -- # op_case
         when Push =>
            if S.Vcount = S.Size then   -- # test_oflow
               raise Constraint_Error;  -- # op_oflow
            end if;

            S.Vcount := S.Vcount + 1;  -- # op_push
            S.Values (S.Vcount) := V;  -- # op_push

         when Pop =>
            if S.Vcount = 0 then       -- # test_uflow
               raise Constraint_Error; -- # op_uflow
            end if;

            V := S.Values (S.Vcount);  -- # op_pop
            S.Vcount := S.Vcount - 1;  -- # op_pop
      end case;
   exception
      when Constraint_Error =>
         pragma Annotate                           -- # op_handler
           (Xcov, Exempt_On, "exception handler"); -- # op_handler
         S.Ecount := S.Ecount + 1;                 -- # op_handler
         pragma Annotate (Xcov, Exempt_Off);       -- # op_handler
   end;

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

   function  Errcount (S : in Stack) return Natural is
   begin
      return S.Ecount; -- # err_body
   end;

end;
