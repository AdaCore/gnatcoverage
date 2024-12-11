pragma Ada_2012;

with With_Decls; use With_Decls;

procedure Call_With_Decls
is
    Dummy : Integer := 0;                                            -- # stmt
    F     : Boolean := False;                                        -- # stmt
begin
    --  For each subprogram, make one call executed and another not

    --------------------
    -- Executed calls --
    --------------------

    --  Functions
    Dummy := Func                                                     -- # fun
           + Fun_Param (8)                                            -- # fun
           + Fun_Param;                                               -- # fun
    Dummy := Expr_Func                                                -- # fun
           - Expr_Func_Param                                          -- # fun
           - Expr_Func_Param (-100);                                  -- # fun
    Dummy := Over (1);                                                -- # fun
    F := Over (10, 2);                                                -- # fun

    --  Procedures
    Proc;                                                             -- # fun
    Proc_Param (Dummy);                                               -- # fun
    Proc_Param (Dummy, False);                                        -- # fun
    Over (5);                                                         -- # fun
    Over (1, 2);                                                      -- # fun

    ------------------------
    -- Non-executed calls --
    ------------------------

    F := False and then F;                                            -- # set_f
    if F then                                                         -- # if_cond
       --  Functions
        Dummy :=                                                      -- # v_stmt
            Func                                                      -- # v_fun
          + Fun_Param (8)                                             -- # v_fun
          + Fun_Param;                                                -- # v_fun
        Dummy :=                                                      -- # v_stmt
            Expr_Func                                                 -- # v_fun
          - Expr_Func_Param                                           -- # v_fun
          - Expr_Func_Param (-100);                                   -- # v_fun
        Dummy := Over (1);                                            -- # v_sfun
        F := Over (10, 2);                                            -- # v_sfun

       --  Procedures
       --  Call statements that are not executed are simply reported as
       --  statement violations.
       Proc;                                                          -- # v_cstmt
       Proc_Param (Dummy);                                            -- # v_cstmt
       Proc_Param (Dummy, False);                                     -- # v_cstmt
       Over (5);                                                      -- # v_cstmt
       Over (1, 2);                                                   -- # v_cstmt
    end if;

end Call_With_Decls;
