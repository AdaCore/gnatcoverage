pragma Ada_2012;

procedure Nested
is
    I : Integer := 1;                                                 -- # stmt

    function B (X : Integer := 5; Y : Boolean := True) return Integer -- # fun
    is
    begin
        if Y then                                                     -- # stmt
            return X;                                                 -- # stmt
        else
            return X;                                                 -- # stmt
        end if;
    end B;

begin
   I :=                                                               -- # stmt
     B                                                                -- # call
       (B                                                             -- # call
           (B),                                                       -- # call
       False);                                                        -- # call

   if I = 42 then                                                     -- # if
      I :=                                                            -- # v_stmt
        B                                                             -- # v_call
          (B                                                          -- # v_call
              (B),                                                    -- # v_call
          False);                                                     -- # v_false
   end if;

end Nested;
