pragma Ada_2012;

package body Pkg is
    package body A is
        package body B is
            procedure P (X : out Integer) is            -- # v_fun
            begin
                X := 42;                                -- # v_stmt
            end P;

            function P (X : Integer) return Integer is  -- # v_fun
            begin
                return 42;                              -- # v_stmt
            end P;
        end B;
    end A;
end Pkg;
