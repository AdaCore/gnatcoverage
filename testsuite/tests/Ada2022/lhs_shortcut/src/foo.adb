pragma Ada_2022;

procedure Foo (B, C : Boolean) is
   A : Boolean := B;                    -- # stmt
begin
   A := @;                              -- # stmt
   A := (if @ then @ else @)       ;    -- # dec
   A := (if @ or else C then @ else @); -- # mcdc
end Foo;
