with Doraise; use Doraise;

package body Ctxt is

   procedure Case1 (Rk : Raise_Kind; Rm : Raise_Mode);
   --  Trigger an inner raise and handle within the same handler

   procedure Case2 (Rk : Raise_Kind; Rm : Raise_Mode);
   procedure Case2_Helper (Rk : Raise_Kind; Rm : Raise_Mode);
   --  Trigger an inner raise from within a handler and handle
   --  one level up.

   procedure Run (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      Case1 (Rk, Rm); -- # pre
      Case2 (Rk, Rm); -- # pre
   end;

   procedure Case1 (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      X := X + 1; -- # pre

      --  Arrange to reach a handler ...
      raise Constraint_Error; -- # pre
   exception
      when others =>
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

   --

   procedure Case2 (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      X := X + 1; -- # pre
      Case2_Helper (Rk, Rm);  -- # pre
   exception
      when Constraint_Error =>
         Handled := Handled + 1; -- # ce_handler
      when Program_Error =>
         Handled := Handled + 1; -- # pe_handler
      when User_Error =>
         Handled := Handled + 1; -- # ue_handler
   end;

   procedure Case2_Helper (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      --  Arrange to reach a handler ...
      raise Constraint_Error; -- # pre
   exception
      when others =>
         begin
            X := X + 1; -- # pre
            Dispatch (Rk, Rm); -- # run
            X := X + 1; -- # post
         end;
   end;
end;
