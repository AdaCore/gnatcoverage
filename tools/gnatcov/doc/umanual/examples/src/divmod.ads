------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

procedure Divmod
  (X, Y : Integer; Value : out Integer;
   Divides : out Boolean; Tell : Boolean);
--  Compute X / Y into VALUE and set DIVIDES to indicate whether Y divides X.
--  Output a note when so and a request to TELL is issued.
