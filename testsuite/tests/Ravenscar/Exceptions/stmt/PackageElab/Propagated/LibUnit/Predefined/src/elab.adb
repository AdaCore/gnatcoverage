pragma Unsuppress (All_Checks);

with Support; use Support;
with Global; use Global;
with SubP; use SubP;
procedure Elab (Val: Integer) is
begin
   declare
      X : Integer ;                             -- # decl
      package Pack is
         I : integer range 2..10 := func(Val);  -- # implicit_violation
      end Pack;
   begin
      X := Identity (5);                         -- # no_imp_violation
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
