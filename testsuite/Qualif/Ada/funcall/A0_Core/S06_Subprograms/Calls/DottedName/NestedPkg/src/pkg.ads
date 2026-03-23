pragma Ada_2012;

package Pkg is
    package A is
        package B is
            function F return Boolean is (False); -- # stmt
            procedure P (X : out Integer);
            function P (X : Integer) return Integer;
        end B;
    end A;
end Pkg;
