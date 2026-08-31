pragma Assertion_Policy (Check);
pragma Ada_2022;

with Ada.Assertions;

procedure Postcond
is
    function A (I : Integer) return Integer
        with Post => A'Result /= 0;                       -- # post_asp

    function A (I : Integer) return Integer is
    begin
        return I;                                         -- # ko
    end A;

    --  Same contracts, expressed as pragmas rather than as aspects

    function B (I : Integer) return Integer;
    pragma Precondition (I /= 0);                         -- # pre_prag
    pragma Postcondition (B'Result /= 0 or else I = 0);   -- # post_prag

    function B (I : Integer) return Integer is
    begin
        return I;                                         -- # b_ret
    end B;

    F : Boolean := False;                                 -- # stmt
    Dummy : Integer := 0;                                 -- # stmt
    Res : Integer := 0;                                   -- # stmt
begin
    --  Evaluate B's postcondition to True through its first condition, so
    --  that the second one is left uncovered for ATCC.

    Res := B (1);                                         -- # stmt

    begin
        Dummy := B (0);                                   -- # stmt
    exception
        when Ada.Assertions.Assertion_Error => null;      -- # stmt
    end;

    begin
        pragma Assert (Dummy /= 0);                       -- # assert
        Dummy := A (0);                                   -- # ko
    exception
        when Ada.Assertions.Assertion_Error => null;      -- # stmt
    end;
end Postcond;
