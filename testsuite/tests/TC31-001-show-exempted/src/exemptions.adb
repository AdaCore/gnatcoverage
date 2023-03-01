procedure Exemptions (X : in out Positive) is
begin
    if X = 1 then                                   -- # if_1
        X := X + 1;                                 -- # inc_1
    end if;

    if X = 3 then                                  -- # if_2
        X := X + 1;                                -- # inc_2
    end if;

    pragma Annotate (Xcov, Exempt_On, "Exempt 1"); -- # exempt1
    if X = 3 then                                  -- # exempt1
        X := X + 1;                                -- # exempt1
        X := X + 1;                                -- # exempt1
    end if;                                        -- # exempt1
    pragma Annotate (Xcov, Exempt_Off);            -- # exempt1

    pragma Annotate (Xcov, Exempt_On, "Exempt 2"); -- # exempt2
    if X = 3 then                                  -- # exempt2
        X := X + 1;                                -- # exempt2
        X := X + 1;                                -- # exempt2
        X := X + 1;                                -- # exempt2
        X := X + 1;                                -- # exempt2
    end if;                                        -- # exempt2
    pragma Annotate (Xcov, Exempt_Off);            -- # exempt2
end Exemptions;
