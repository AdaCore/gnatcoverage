pragma Ada_2012;

with Pkg_Type_Invariant; use Pkg_Type_Invariant;

procedure Main is
    Dummy_R : Bool_Record := Make_Bool_Record (True);

    function Id (R : Bool_Record) return Bool_Record is (R);
    pragma Precondition (Get_Value (R));
    pragma Postcondition (Get_Value (Id'Result));
begin
    Dummy_R := Id (Dummy_R);
end;
