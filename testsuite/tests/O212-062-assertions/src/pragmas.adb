pragma Assertion_Policy (Check);
pragma Ada_2012;

with Ada.Assertions;

procedure Pragmas (T : Boolean := True; F : Boolean := False)
is
    function Id (B : Boolean) return Boolean is (B);            -- # id
begin
    begin
        --  Pass
        pragma Assume (T and then (F or else T));               -- # assume

        pragma Check (Precondition, F or else not T or else T); -- # c_pre
        pragma Check (Postcondition, not Id (F));               -- # c_post
        pragma Check (Type_Invariant, T or else Id (T));        -- # c_ti
        pragma Check (Invariant, T);                            -- # c_inv
        pragma Check (Assertion, T or else T);                  -- # c_assert

        pragma Assert_And_Cut (T or else (F or else T));        -- # cut

        -- Fail
        pragma Assume (Id (F) or else F);                       -- # fail
    exception
        when Ada.Assertions.Assertion_Error => null;            -- # catch
    end;
end Pragmas;
