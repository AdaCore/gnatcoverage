pragma Assertion_Policy (Check);
pragma Ada_2012;

with Ada.Assertions;

procedure Expr_Func
is
    --  Expression function without a prior declaration.
    function Foo (X : Integer) return Integer is (X)   -- # foo_decl
        with Pre  => X >= 0,                           -- # foo_pre
             Post => Foo'Result = X;                   -- # foo_post

    --  Expression function with aspects on prior declaration
    function Bar (X : Integer) return Integer
        with Pre  => X > 0,                            -- # bar_pre
             Post => Bar'Result = X and then X = 0;    -- # bar_post

    function Bar (X : Integer) return Integer is (X);  -- # bar_def

    Dummy : Integer;                                   -- # dummy_decl
begin
    Dummy := Foo (0);                                  -- # foo_call

    begin
        Dummy := Bar (42);                             -- # bar_call
    exception
        when Ada.Assertions.Assertion_Error => null;   -- # catch
    end;
end Expr_Func;
