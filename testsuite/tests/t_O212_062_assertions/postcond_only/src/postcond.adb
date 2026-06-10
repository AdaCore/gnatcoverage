pragma Assertion_Policy (Check);
pragma Ada_2012;

with Ada.Assertions;

procedure Postcond
is
    function A (I : Integer) return Integer
        with Post => A'Result /= 0;                       -- # post

    function A (I : Integer) return Integer is
    begin
        return I;                                         -- # ko
    end A;

    F : Boolean := False;                                 -- # stmt
    Dummy : Integer := 0;                                 -- # stmt
begin
    begin
        pragma Assert (Dummy /= 0);                       -- # assert
        Dummy := A (0);                                   -- # ko
    exception
        when Ada.Assertions.Assertion_Error => null;      -- # stmt
    end;
end Postcond;
