pragma Ada_2012;
pragma Assertion_Policy (Check);

package Nested is

    T : Boolean := True;                                        -- # success
    F : Boolean := False;                                       -- # success

    function One return Integer
      with Pre => (T and then T) or else F,                     -- # fail
           Post => One'Result = 1 and then                      -- # fail
           (T and then                                          -- # fail
               (T or else                                       -- # fail
                   (F or else T)));                             -- # fail

    function Two return Integer
      with Pre => (T and then (if T then T else T)) or else F,  -- # fail
           Post => (if Two'Result = 2 and then                  -- # success
             (T and then                                        -- # success
                (F or else (T or else F)))                      -- # success
            then T                                              -- # success
            else F);                                            -- # success

    function Three return Integer
      with Post => (if (Three'Result = 2 and then (T and then   -- # success
          (if T then T else F)))                                -- # success
            then (T and then T)                                 -- # success
            else T);                                            -- # success
end Nested;
