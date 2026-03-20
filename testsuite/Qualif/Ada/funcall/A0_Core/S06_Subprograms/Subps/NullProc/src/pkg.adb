package body Pkg is
    procedure P1 is null;                            -- # p
    procedure P2 (I : Integer; J : Natural) is null; -- # p
end Pkg;
