pragma Ada_2022;

package body Pkg is
    function Foo (A, B : Natural) return Boolean is
    begin
        return (for some I in A .. B => I = 10); -- # expr
    end Foo;
end Pkg;
