pragma Assertion_Policy (Check);
pragma Ada_2022;

with Silent_Last_Chance;

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

    Dummy := Bar (42);                                 -- # bar_call
end Expr_Func;
