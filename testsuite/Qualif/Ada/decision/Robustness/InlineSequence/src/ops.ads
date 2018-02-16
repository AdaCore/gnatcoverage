package Ops is
  type Uns64 is mod 2 ** 64;
  type Int64 is range -2 ** 63 .. 2 ** 63 - 1;

  Rx, Ry : Uns64;
  procedure Process (X, Y : Int64);
  pragma No_Inline (Process);
end;
