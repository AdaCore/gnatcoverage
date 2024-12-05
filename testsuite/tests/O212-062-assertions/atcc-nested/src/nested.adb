pragma Ada_2012;
pragma Assertion_Policy (Check);

package body Nested is

    function One return Integer is
    begin
        pragma Assert                                            -- # success
            (T and then (F or else (T and then (T or else F)))); -- # fail
        return 1;                                                -- # success
    end One;

    --  One of the branches of the if expression is never exercised.
    --  No violation is expected since gnatcov should not look into nested
    --  decisions in assertions (FIXME: for now)
    function Two return Integer is
    begin
        pragma Assert                                            -- # success
            ((if T then T else F) and then (F or else            -- # success
                (T and then (if (T and then F)                   -- # success
                then T                                           -- # success
                else T))));                                      -- # success
        return 2;                                                -- # success
    end Two;

    function Three return Integer is
    begin
        pragma Assert (if (T and then (T and then                -- # success
          (if T then T else F)))                                 -- # success
            then (T and then T)                                  -- # success
            else T);                                             -- # success
        return 3;                                                -- # success
    end Three;

end nested;
