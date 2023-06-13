pragma Unsuppress (All_Checks);

with Support; use Support;
with Global; use Global;
procedure Elab (Val_In_Range: Integer) is
begin

   begin  -- outer
      declare
         X : integer ;                               -- # decl
         package Pack is
            I : integer range 2..10 := Val_In_Range;    -- # violation
         end Pack;
      begin
         X := Identity (5);                         -- # stmt
      exception
         when Constraint_Error =>
            Wrong_Exception_Raised := True;         -- # wrong_handler
         when others =>
            Wrong_Exception_Raised := True;         -- # wrong_handler
      end;
   exception
      when storage_error | program_error =>
         Wrong_Exception_Raised := True;            -- # wrong_handler
   end; --outer
exception
   when Constraint_Error =>
      Correct_Exception_Raised := True;          -- # properly_handled
   when others =>
      Wrong_Exception_Raised := True;            -- # wrong_exception
end Elab;
