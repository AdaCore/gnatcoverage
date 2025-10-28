pragma Ada_2012;

with With_Decls_Subp; use With_Decls_Subp;

procedure Make_Calls                                           -- # test_proc
is
   Dummy : Integer := 0;                                       -- # decl
   F     : Boolean := False;                                   -- # decl
begin
      --  Function calls.
      --  If a function call is not executed, a call violation is reported.
      Dummy :=                                                 -- # dummy
          Func                                                 -- # fcall
        + Fun_Param (8)                                        -- # fcall
        + Fun_Param;                                           -- # fcall
      Dummy :=                                                 -- # dummy
          Expr_Func                                            -- # fcall
        - Expr_Func_Param                                      -- # fcall
        - Expr_Func_Param (-100);                              -- # fcall
      Dummy :=                                                 -- # dummy
          Over (1);                                            -- # fcall
      F :=                                                     -- # dummy
          Over (1, 1);                                         -- # fcall

      --  Procedure calls.
      --  Call statements that are not executed are simply reported as
      --  statement violations.
      Proc;                                                    -- # pcall
      Proc_Param (Dummy);                                      -- # pcall
      Proc_Param (Dummy, F);                                   -- # pcall
      Over (5);                                                -- # pcall
      Over (1, 2);                                             -- # pcall

end Make_Calls;
