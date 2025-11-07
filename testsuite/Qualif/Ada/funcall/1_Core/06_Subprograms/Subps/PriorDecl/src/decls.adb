pragma Ada_2012;

package body Decls is

    --  Define subprograms that have prior declarations.
    --  Any function coverage relative to these subprograms should be linked
    --  to the locations of the subprogram definitions, as opposed to their
    --  prior declarations.

    ---------------
    -- Functions --
    ---------------

    function Func return Integer is                             -- # subp
    begin
        return 42;                                              -- # stmt
    end Func;

    function Fun_Param (X : Integer := 1) return Integer        -- # subp
    is
    begin
        return X;                                               -- # stmt
    end Fun_Param;

    function Expr_Func return Integer                           -- # subp
    is (42);                                                    -- # stmt

    function Expr_Func_Param (X : Integer := 1) return Integer  -- # subp
    is (X);                                                     -- # stmt

    ----------------
    -- Procedures --
    ----------------

    procedure Proc is                                           -- # subp
    begin
        I := I + 1 - 1;                                         -- # stmt
    end Proc;

    procedure Proc_Param (X : out Integer; Y : Boolean := True) -- # subp
    is
    begin
        X := (if Y then 5 else 6);                              -- # stmt
    end Proc_Param;

    ----------------------------
    -- Overloaded subprograms --
    ----------------------------

    function Over (A : Integer) return Integer is               -- # subp
    begin
        return A;                                               -- # stmt
    end Over;

    function Over (A, B : Integer) return Boolean is            -- # subp
    begin
        return A /= B;                                          -- # stmt
    end Over;

    procedure Over (A : Integer) is                             -- # subp
    begin
        I := A;                                                 -- # stmt
    end Over;

    procedure Over (A, B : Integer) is                          -- # subp
    begin
        I := A + B;                                             -- # stmt
    end Over;

end Decls;
