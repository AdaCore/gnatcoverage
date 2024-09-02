pragma Ada_2012;

With Pkg;

procedure Call
is
    Dummy : Integer := 8;       -- #decl
begin
    if Pkg.A.B.F then           -- # if_cond
        Pkg.A.B.P (Dummy);      -- # v_call_stmt
        Dummy := Pkg.A.B.P (2); -- # v_call_expr
    end if;
end Call;
