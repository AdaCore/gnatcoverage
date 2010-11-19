package body Pack is
   package body Inner is separate;

   procedure Update (I : in out Integer) is separate;
   function New_Value (I : Integer) return Integer is separate;

   procedure Non_Separate_Proc (I : in out Integer) is
   begin
      I := I / 2;               -- # stmt
   end Non_Separate_Proc;

end Pack;
