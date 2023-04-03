with Ctxt; use Ctxt;

package body Doraise is

   pragma Unsuppress (All_Checks);

   procedure Do_Raise (Rk : Raise_Kind) is

      -- This is the common procedure called to perform a raise to
      -- be propagated. It is not called at all for Raise_Mode None,
      -- so the "pre" or "run" anchors aren't appropriate here.
      --
      -- do_raise-pre is introduced instead, needed at least for
      -- the declarations below.

      -- Entities to help trigger specific kinds of occurrences

      R0 : Integer range 0 .. 0 := 0; -- # do_raise-pre
      I1 : Integer := 1;              -- # do_raise-pre
      pragma Volatile (I1);

      Vint : Integer;           -- # do_raise-pre
      Vstr : String := "blue";  -- # do_raise-pre
      pragma Volatile (Vstr);

   begin
      X := X + 1; -- # do_raise-pre
      if Rk = Implicit_CE then -- # ice-eval
         R0 := I1; -- # ice-raise

      elsif Rk = Explicit_PE then -- # xpe-eval
         raise Program_Error; -- # xpe-raise

      elsif Rk = Explicit_UE then -- # xue-eval
         raise User_Error; -- # xue-raise

      elsif Rk = From_RTS then -- # rts-eval
         Vint := Integer'Value(Vstr); -- # rts-raise

      end if;
      X := X + 1; -- # do_raise-post
   end;

   procedure Do_Reraise_One (Rk : Raise_Kind) is
   begin
      Do_Raise (Rk); -- # call_reraise_one
   exception
      when Constraint_Error =>
         Handled := Handled + 1; -- # reraise_one_ce_handler
         raise; -- # reraise_one_ce_handler
      when Program_Error =>
         Handled := Handled + 1; -- # reraise_one_pe_handler
         raise; -- # reraise_one_pe_handler
      when User_Error =>
         Handled := Handled + 1; -- # reraise_one_ue_handler
         raise; -- # reraise_one_ue_handler
   end;

   procedure Do_Reraise_Other (Rk : Raise_Kind) is
   begin
      Do_Raise (Rk); -- # call_reraise_other
   exception
      when others =>
         Handled := Handled + 1; -- # reraise_other_handler
         raise; -- # reraise_other_handler
   end;

   procedure Dispatch (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      X := X + 1; -- # pre
      case Rm is -- # pre
         when Straight =>
            Do_Raise (Rk);         -- # call_straight
         when Reraise_Other =>
            Do_Reraise_Other (Rk); -- # call_reraise_other
         when Reraise_One =>
            Do_Reraise_One (Rk);   -- # call_reraise_one
         when None =>
            null; -- # rm_none
      end case;
      X := X + 1; -- # post
   end;
end;
