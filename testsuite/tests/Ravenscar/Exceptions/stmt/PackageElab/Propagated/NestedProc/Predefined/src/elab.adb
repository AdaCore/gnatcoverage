pragma Unsuppress (All_Checks);

with Support; use Support;
with Global; use Global;
procedure Elab (Val: Integer) is

   function func (x: integer) return integer is
   -- x must be in range 1 thru 10
   begin
      if x < 1 or x > 10 then                -- # test
         raise Constraint_Error;               -- # explicit_violation
      else
         return x;                              -- # no_exp_violation
      end if;
   end func;

begin
   declare
      X : Integer range 0..9;                    -- # decl
      package Pack is
         I : integer range 2..10 := func(Val);    -- # implicit_violation
      end Pack;
   begin
      X := Identity (5);                          -- # no_imp_violation
   exception
      when Constraint_Error =>
         Wrong_Exception_Raised := True;         -- # wrong_handler
      when others =>
         Wrong_Exception_Raised := True;         -- # wrong_handler
   end;
exception
   when Constraint_Error =>
      Correct_Exception_Raised := True;          -- # properly_handled
   when others =>
      Wrong_Exception_Raised := True;            -- # wrong_exception
end Elab;
