package Support is
   procedure Assert (Cond : Boolean);
   Pragma Export(C, Assert, "assert");
end;
