pragma Assertion_Policy (Check);
pragma Ada_2012;

with Ada.Assertions;

procedure Postcond2012
is
    function A (I : Integer) return Integer
        with Post => A'Result /= 0 or else I = 0; -- # post_a

    function A (I : Integer) return Integer is
    begin
        return I;                                 -- # a_body
    end A;

   Dummy : Integer;                               -- # stmt
begin
    Dummy := A (1);                               -- # stmt
    Dummy := A (0);                               -- # stmt
end Postcond2012;
