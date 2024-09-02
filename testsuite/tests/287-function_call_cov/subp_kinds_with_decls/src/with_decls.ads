pragma Ada_2012;

package With_Decls is

    I : Integer := 1;                                  -- # stmt

    --  No parameters
    function Func return Integer;
    procedure Proc;

    --  With parameters
    function Fun_Param (X : Integer := 1) return Integer;
    procedure Proc_Param (X : out Integer; Y : Boolean := True);

    --  Expression function
    function Expr_Func return Integer;
    function Expr_Func_Param (X : Integer := 1) return Integer;

    --  Overloaded
    function Over (A : Integer) return Integer;
    function Over (A, B : Integer) return Boolean;
    procedure Over (A : Integer);
    procedure Over (A, B : Integer);

end With_Decls;
