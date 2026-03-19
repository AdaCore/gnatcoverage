procedure Main
is
    T : Boolean := True;
    F : Boolean := False;

    function Concat (S1, S2, S3: String) return String is
    begin
        return S1 & S2 & S3;
    end Concat;

    procedure Test (B : Boolean) is
    begin
        if B
            and then
                (T
                or else F)
        then
            if F
                or else
                F
            then
                declare
                    Dummy : String := Concat
                    (
                    "B",
                    "C",
                    "D"
                    );
                begin
                    null;
                end;
            end if;
        end if;
    end Test;
begin
    Test (T);
    Test (F);
end Main;
