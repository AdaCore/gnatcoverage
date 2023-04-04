with Doraise; use Doraise;

package body Ctxt is
   procedure Run (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      X := X + 1; -- # pre
      Dispatch (Rk, Rm); -- # run
      X := X + 1; -- # post
   exception
      when Constraint_Error =>
         Handled := Handled + 1; -- # ce_handler
      when Program_Error =>
         Handled := Handled + 1; -- # pe_handler
      when User_Error =>
         Handled := Handled + 1; -- # ue_handler
   end;
end;
