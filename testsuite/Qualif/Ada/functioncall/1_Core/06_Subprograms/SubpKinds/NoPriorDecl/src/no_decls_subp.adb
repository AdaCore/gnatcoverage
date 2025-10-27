pragma Ada_2012;

procedure No_Decls_Subp (Make_Calls : Boolean)                 -- # test_proc
is
   I : Integer := 8;

   ---------------
   -- Functions --
   ---------------

   --  Function with no parameter
   function Func return Integer is                             -- # subp
   begin
       return 42;                                              -- # stmt
   end Func;

   --  Function with one parameter
   function Fun_Param (X : Integer := 1) return Integer        -- # subp
   is
   begin
       return X;                                               -- # stmt
   end Fun_Param;

   --  Expression function with no parameter
   function Expr_Func return Integer                           -- # subp
   is (42);                                                    -- # stmt

   --  Expression function with one parameter
   function Expr_Func_Param (X : Integer := 1) return Integer  -- # subp
   is (X);                                                     -- # stmt

   ----------------
   -- Procedures --
   ----------------

   --  Procedure with no parameter
   procedure Proc is                                           -- # subp
   begin
       I := I + 1 - 1;                                         -- # stmt
   end Proc;

   --  Procedure with one in parameter and one out parameter
   procedure Proc_Param (X : out Integer; Y : Boolean := True) -- # subp
   is
   begin
       X := (if Y then 5 else 6);                              -- # stmt
   end Proc_Param;

   ----------------------------
   -- Overloaded subprograms --
   ----------------------------

   --  One-parameter function version of an overloaded subprogram
   function Over (A : Integer) return Integer is               -- # subp
   begin
       return A;                                               -- # stmt
   end Over;

   --  Two-parameter function version of an overloaded subprogram
   function Over (A, B : Integer) return Boolean is            -- # subp
   begin
       return A /= B;                                          -- # stmt
   end Over;

   --  One-parameter procedure version of an overloaded subprogram
   procedure Over (A : Integer) is                             -- # subp
   begin
       I := A;                                                 -- # stmt
   end Over;

   --  Two-parameter procedure version of an overloaded subprogram
   procedure Over (A, B : Integer) is                          -- # subp
   begin
       I := A + B;                                             -- # stmt
   end Over;

   Dummy : Integer := 0;                                       -- # decl
   F     : Boolean := False;                                   -- # decl
begin

   if Make_Calls then                                          -- # if_cond

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
   end if;

end No_Decls_Subp;
