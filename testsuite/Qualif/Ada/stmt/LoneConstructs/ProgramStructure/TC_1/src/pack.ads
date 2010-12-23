package Pack is
   package Inner is
      I : Integer;

      function Fun (I : Integer) return Integer;
   end Inner;

   procedure Update (I : in out Integer);
   function New_Value (I : Integer) return Integer;

   procedure Non_Separate_Proc (I : in out Integer);
end Pack;
