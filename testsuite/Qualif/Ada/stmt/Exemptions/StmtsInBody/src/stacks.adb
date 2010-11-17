package body Stacks is

   procedure On (S : in out Stack; Op : Op_Kind; V : in out Integer) is
   begin
      case Op is -- # op_case
         when Push =>
            pragma Annotate                      -- # op_oflow
              (Xcov, Exempt_On, "no overflow");  -- # op_oflow
            if S.Vcount = S.Size then            -- # op_oflow
               raise Constraint_Error;           -- # op_oflow
            end if;                              -- # op_oflow
            pragma Annotate (Xcov, Exempt_Off);  -- # op_oflow

            S.Vcount := S.Vcount + 1;  -- # op_push
            S.Values (S.Vcount) := V;  -- # op_push

         when Pop =>
            pragma Annotate                      -- # op_uflow
              (Xcov, Exempt_On, "no underflow"); -- # op_uflow
            if S.Vcount = 0 then                 -- # op_uflow
               raise Constraint_Error;           -- # op_uflow
            end if;                              -- # op_uflow
            pragma Annotate (Xcov, Exempt_Off);  -- # op_uflow

            V := S.Values (S.Vcount);  -- # op_pop
            S.Vcount := S.Vcount - 1;  -- # op_pop
      end case;
   exception
      when Constraint_Error =>
         S.Ecount := S.Ecount + 1; -- # op_handler
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
