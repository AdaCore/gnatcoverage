procedure Main
is
    function C_Func (A : Integer) return Integer;
    pragma Import (C, C_Func, "c_func");

    Dummy_I : Integer := 1;
begin
    Dummy_I := C_Func (Dummy_I);
end;
