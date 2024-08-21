pragma Ada_2012;

procedure No_Decls_Not_Called is                                -- # p

    I : Integer := 8;                                           -- # stmt

    function Func return Integer is                             -- # fun
    begin
        return 42;                                              -- # stmt
    end Func;

    procedure Proc is                                           -- # fun
    begin
        I := I + 1 - 1;                                         -- # stmt
    end Proc;

    function Fun_Param (X : Integer := 1) return Integer        -- # fun
    is
    begin
        return X;                                               -- # stmt
    end Fun_Param;

    procedure Proc_Param (X : out Integer; Y : Boolean := True) -- # fun
    is
    begin
        X := (if Y then 5 else 6);                              -- # stmt
    end Proc_Param;

    function Expr_Func return Integer                           -- # fun
    is (42);                                                    -- # stmt

    function Expr_Func_Param (X : Integer := 1) return Integer  -- # fun
    is (X);                                                     -- # stmt

    function Over (A : Integer) return Integer is               -- # fun
    begin
        return A;                                               -- # stmt
    end Over;

    function Over (A, B : Integer) return Boolean is            -- # fun
    begin
        return A /= B;                                          -- # stmt
    end Over;

    procedure Over (A : Integer) is                             -- # fun
    begin
        I := A;                                                 -- # stmt
    end Over;

    procedure Over (A, B : Integer) is                          -- # fun
    begin
        I := A + B;                                             -- # stmt
    end Over;

begin
    null;                                                       -- # stmt
end No_Decls_Not_Called;
