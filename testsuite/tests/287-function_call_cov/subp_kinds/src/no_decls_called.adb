pragma Ada_2012;

procedure No_Decls_Called is                                    -- # p

    I : Integer := 8;

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

    Dummy : Integer := 0;                                       -- # decl
    F     : Boolean := False;                                   -- # decl
begin
    --  For each subprogram, make one call executed and another not

    --------------------
    -- Executed calls --
    --------------------

    --  Functions
    Dummy := Func                                               -- # call
           + Fun_Param (8)                                      -- # call
           + Fun_Param;                                         -- # call
    Dummy := Expr_Func                                          -- # call
           - Expr_Func_Param                                    -- # call
           - Expr_Func_Param (-100);                            -- # call
    Dummy := Over (1);                                          -- # call
    F := Over (10, 2);                                          -- # call

    --  Procedures
    Proc;                                                       -- # call
    Proc_Param (Dummy);                                         -- # call
    Proc_Param (Dummy, False);                                  -- # call
    Over (5);                                                   -- # call
    Over (1, 2);                                                -- # call

    ------------------------
    -- Non-executed calls --
    ------------------------

    F := False and then F;                                      -- # set_f
    if F then                                                   -- # if_cond
       --  Functions
        Dummy :=                                                -- # v_dummy
            Func                                                -- # v_call
          + Fun_Param (8)                                       -- # v_call
          + Fun_Param;                                          -- # v_call
        Dummy :=                                                -- # v_dummy
            Expr_Func                                           -- # v_call
          - Expr_Func_Param                                     -- # v_call
          - Expr_Func_Param (-100);                             -- # v_call
        Dummy := Over (1);                                      -- # v_scall
        F := Over (10, 2);                                      -- # v_scall

       --  Procedures
       --  Call statements that are not executed are simply reported as
       --  statement violations.
       Proc;                                                    -- # v_stmt
       Proc_Param (Dummy);                                      -- # v_stmt
       Proc_Param (Dummy, False);                               -- # v_stmt
       Over (5);                                                -- # v_stmt
       Over (1, 2);                                             -- # v_stmt
    end if;

end No_Decls_Called;
