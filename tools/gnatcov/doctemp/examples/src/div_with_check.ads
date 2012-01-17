------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

function Div_With_Check (X, Y : Integer) return Integer;
--  If Y /= 0, divide X by Y and return the result. Raise
--  Program_Error otherwise.
