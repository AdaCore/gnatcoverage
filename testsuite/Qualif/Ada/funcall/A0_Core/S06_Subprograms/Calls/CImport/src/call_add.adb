pragma Ada_2012;

procedure Call_Add                                -- # fun
is
    function Add (X, Y : Integer) return Integer;
    pragma Import (C, Add);

    Dummy : Integer := 0;                         -- # stmt
begin
    if Add (40, 2) /= 42 then                     -- # if_1
        Dummy := Add (42, 2);                     -- # call_1
    end if;

    if Dummy = 50 then                            -- # if_2
        if Add (40, 2) /= 42 then                 -- # if_3
            Dummy := Add (42, 2);                 -- # call_2
        end if;
    end if;

    if Add (40, 2) = 42 then                      -- # if_4
        Dummy := Add (42, 2);                     -- # call_3
    end if;

end Call_Add;
