package Common is
   procedure Check_Pos (X : Integer);
   --  A non-ghost subprogram in a non-ghost package, which lets every
   --  test exercise regular expectations without any ghost entity in
   --  sight within the unit.
end;
