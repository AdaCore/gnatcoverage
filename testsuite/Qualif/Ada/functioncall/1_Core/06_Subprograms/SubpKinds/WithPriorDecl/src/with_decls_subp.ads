package With_Decls_Subp is

    I : Integer := 8;

   --------------
   -- Functions --
   ---------------

   --  Function with no parameter
   function Func return Integer;

   --  Function with one parameter
   function Fun_Param (X : Integer := 1) return Integer;

   --  Expression function with no parameter
   function Expr_Func return Integer;

   --  Expression function with one parameter
   function Expr_Func_Param (X : Integer := 1) return Integer;

   ----------------
   -- Procedures --
   ----------------

   --  Procedure with no parameter
   procedure Proc;

   --  Procedure with one in parameter and one out parameter
   procedure Proc_Param (X : out Integer; Y : Boolean := True);

   -----------------------------------------
   -- Overloaded functions and procedures --
   -----------------------------------------

   --  One-parameter function version of an overloaded subprogram
   function Over (A : Integer) return Integer;

   --  Two-parameter function version of an overloaded subprogram
   function Over (A, B : Integer) return Boolean;

   --  One-parameter procedure version of an overloaded subprogram
   procedure Over (A : Integer);

   --  Two-parameter procedure version of an overloaded subprogram
   procedure Over (A, B : Integer);

end With_Decls_Subp;
